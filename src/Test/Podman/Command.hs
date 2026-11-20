-- |
-- Module      : Test.Podman.Command
-- Copyright   : (c) 2026 JP Rupp
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Running the @podman@ command, and finding out whether it can run.
--
-- = Why the command line
--
-- Podman has a REST API on a Unix socket too, but it is a service that has
-- to be started, and its JSON changes between versions more than the flags
-- of the command do. The command is there wherever Podman is installed, and
-- what this library asks of it is a handful of flags that have not changed
-- in years. Each call is a process, which costs a few milliseconds: nothing
-- next to the start of a container.
--
-- = Why every call has a deadline
--
-- A test that hangs is worse than one that fails: CI kills it after an hour
-- and says nothing about why. A call to @podman@ can hang when its storage is
-- locked by a crashed process, or when a registry does not answer. So every
-- call here gives up after a deadline, and throws a t'PodmanError' that says
-- what it ran and what Podman printed.
--
-- = Which podman
--
-- The program is the one named by @PODMAN_TEST_PODMAN@, if it is set, or the
-- first @podman@ on the @PATH@, or else the one of the machine: in
-- @\/usr\/bin@, @\/usr\/local\/bin@, or @\/run\/current-system\/sw\/bin@
-- on NixOS ('findPodman'). The last is for builds in a pure Nix shell, as
-- @stack --nix@ makes them: the shell keeps the files of the machine but
-- leaves only packages of Nix on the @PATH@, so the tests would not find
-- the Podman that the machine has.
--
-- The machine's Podman cannot come from Nix: rootless Podman needs
-- @newuidmap@ and @newgidmap@, which must be setuid, and nothing in the
-- store of Nix can be. So it runs in an environment of its own
-- ('podmanEnvironment'): the folders of the machine at the end of the
-- @PATH@, where it finds those programs and its other helpers, NixOS's
-- @\/run\/wrappers\/bin@ first among them, and no @LD_LIBRARY_PATH@ or
-- @LD_PRELOAD@, which a shell of Nix may set, and which would make a
-- program of the machine load libraries of Nix. The program is looked for
-- once in a run.
module Test.Podman.Command
  ( -- * Running podman
    podman,
    podmanWithin,
    podmanStatus,
    PodmanError (..),

    -- * Whether podman can run
    podmanUsable,
    podmanPath,

    -- * Which podman
    findPodman,
    podmanEnvironment,
    machineFolders,
  )
where

import Control.Exception (Exception, throwIO)
import Data.Char (isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (dropWhileEnd, intercalate)
import System.Directory (doesFileExist, executable, getPermissions)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (searchPathSeparator, splitSearchPath, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- | A call to @podman@ that failed or ran out of time, with what it was
-- given and what it printed.
data PodmanError
  = -- | it exited with this code, and printed this on its standard error
    PodmanFailed ![String] !Int !String
  | -- | it did not end within so many seconds
    PodmanTimedOut ![String] !Int
  | -- | there is no podman to run, for this reason ('findPodman')
    PodmanMissing !String
  deriving (Eq)

instance Show PodmanError where
  show (PodmanFailed args code err) =
    "podman " <> unwords args <> " exited with " <> show code <> ": " <> trim err
  show (PodmanTimedOut args secs) =
    "podman " <> unwords args <> " did not end within " <> show secs <> " seconds"
  show (PodmanMissing why) = why

instance Exception PodmanError

-- | Run @podman@ with these arguments, and give what it printed on its
-- standard output, without the white space at its end. Throws a
-- t'PodmanError' if it fails, or if it takes more than two minutes.
podman :: [String] -> IO String
podman = podmanWithin 120

-- | 'podman' with a deadline of so many seconds, for what can take longer,
-- such as pulling or building an image.
podmanWithin :: Int -> [String] -> IO String
podmanWithin secs args =
  podmanStatus secs args >>= \case
    (ExitSuccess, out, _) -> return (trim out)
    (ExitFailure code, _, err) -> throwIO (PodmanFailed args code err)

-- | Run @podman@ and give its exit code and both of its outputs, whether it
-- succeeded or not, for a call whose failure is an answer, such as
-- @podman image exists@. Throws only if it does not end in time.
podmanStatus :: Int -> [String] -> IO (ExitCode, String, String)
podmanStatus secs args =
  found >>= \case
    Left why -> throwIO (PodmanMissing why)
    Right (program, environment) -> do
      let process = (proc program args) {env = Just environment}
      timeout (secs * 1000000) (readCreateProcessWithExitCode process "") >>= \case
        Nothing -> throwIO (PodmanTimedOut args secs)
        Just result -> return result

-- | The podman of this run and the environment to run it in, looked for at
-- the first call and kept.
found :: IO (Either String (FilePath, [(String, String)]))
found =
  readIORef foundRef >>= \case
    Just r -> return r
    Nothing -> do
      environment <- getEnvironment
      program <- findPodman environment
      let r = (\path -> (path, podmanEnvironment environment)) <$> program
      atomicModifyIORef' foundRef (\_ -> (Just r, r))

foundRef :: IORef (Maybe (Either String (FilePath, [(String, String)])))
foundRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE foundRef #-}

-- | The podman that the calls of this run use, if there is one.
podmanPath :: IO (Maybe FilePath)
podmanPath = either (const Nothing) (Just . fst) <$> found

-- | The folders of the machine where its programs are, in the order in which
-- they are looked in: NixOS's setuid wrappers and its system, then those
-- of every other Linux.
machineFolders :: [FilePath]
machineFolders =
  ["/run/wrappers/bin", "/run/current-system/sw/bin", "/usr/local/bin", "/usr/bin", "/bin", "/usr/sbin", "/sbin"]

-- | Find the podman to run, in an environment: the program that
-- @PODMAN_TEST_PODMAN@ names, if it is set; the first @podman@ on the
-- @PATH@; or the first in the folders of the machine where Podman is
-- installed. Gives why there is none, if there is none.
findPodman :: [(String, String)] -> IO (Either String FilePath)
findPodman environment = case lookup "PODMAN_TEST_PODMAN" environment of
  Just named
    | not (null named) ->
        runnable named >>= \case
          True -> return (Right named)
          False -> return (Left ("PODMAN_TEST_PODMAN names " <> named <> ", which is not a program that can run"))
  _ -> first (map (</> "podman") (onPath <> installed))
  where
    onPath = maybe [] splitSearchPath (lookup "PATH" environment)
    installed = ["/usr/bin", "/usr/local/bin", "/run/current-system/sw/bin"]
    first = \case
      [] ->
        return . Left $
          "podman is not on the PATH, nor in "
            <> intercalate ", " installed
            <> ": install Podman, or name it with PODMAN_TEST_PODMAN"
      p : ps -> runnable p >>= \ok -> if ok then return (Right p) else first ps
    runnable p = do
      exists <- doesFileExist p
      if exists then executable <$> getPermissions p else return False

-- | The environment to run podman in, from that of the tests: the folders of
-- the machine at the end of the @PATH@, those that it does not have yet,
-- so that podman finds @newuidmap@ and its other helpers where the machine
-- has them; and no @LD_LIBRARY_PATH@ or @LD_PRELOAD@, which would make a
-- program of the machine load the libraries of a shell of Nix.
podmanEnvironment :: [(String, String)] -> [(String, String)]
podmanEnvironment environment =
  ("PATH", path) : [kv | kv@(k, _) <- environment, k `notElem` ["PATH", "LD_LIBRARY_PATH", "LD_PRELOAD"]]
  where
    current = maybe [] splitSearchPath (lookup "PATH" environment)
    path = intercalate [searchPathSeparator] (current <> [d | d <- machineFolders, d `notElem` current])

-- | Whether this machine can run the containers of the tests, and why not if
-- it cannot: @Right ()@, or @Left@ a reason to show for the tests that are
-- skipped.
--
-- It cannot when @PODMAN_TEST_SKIP@ is set, to any value, when there is no
-- podman to run ('findPodman'), or when @podman info@ fails, as it does
-- when the user has no subordinate IDs for rootless containers. A test framework can
-- mark the tests as pending with the reason, so that a machine without
-- Podman runs the rest of the tests and says what it left out.
podmanUsable :: IO (Either String ())
podmanUsable =
  lookupEnv "PODMAN_TEST_SKIP" >>= \case
    Just _ -> return (Left "PODMAN_TEST_SKIP is set")
    Nothing ->
      found >>= \case
        Left why -> return (Left why)
        Right _ ->
          podmanStatus 60 ["info", "--format", "{{.Host.Arch}}"] >>= \case
            (ExitSuccess, _, _) -> return (Right ())
            (_, _, err) -> return (Left ("podman info failed: " <> trim err))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
