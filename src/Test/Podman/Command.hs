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
module Test.Podman.Command
  ( -- * Running podman
    podman,
    podmanWithin,
    podmanStatus,
    PodmanError (..),

    -- * Whether podman can run
    podmanUsable,
  )
where

import Control.Exception (Exception, throwIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | A call to @podman@ that failed or ran out of time, with what it was
-- given and what it printed.
data PodmanError
  = -- | it exited with this code, and printed this on its standard error
    PodmanFailed ![String] !Int !String
  | -- | it did not end within so many seconds
    PodmanTimedOut ![String] !Int
  deriving (Eq)

instance Show PodmanError where
  show (PodmanFailed args code err) =
    "podman " <> unwords args <> " exited with " <> show code <> ": " <> trim err
  show (PodmanTimedOut args secs) =
    "podman " <> unwords args <> " did not end within " <> show secs <> " seconds"

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
  timeout (secs * 1000000) (readProcessWithExitCode "podman" args "") >>= \case
    Nothing -> throwIO (PodmanTimedOut args secs)
    Just result -> return result

-- | Whether this machine can run the containers of the tests, and why not if
-- it cannot: @Right ()@, or @Left@ a reason to show for the tests that are
-- skipped.
--
-- It cannot when @PODMAN_TEST_SKIP@ is set, to any value, when there is no
-- @podman@ on the @PATH@, or when @podman info@ fails, as it does when the
-- user has no subordinate IDs for rootless containers. A test framework can
-- mark the tests as pending with the reason, so that a machine without
-- Podman runs the rest of the tests and says what it left out.
podmanUsable :: IO (Either String ())
podmanUsable =
  lookupEnv "PODMAN_TEST_SKIP" >>= \case
    Just _ -> return (Left "PODMAN_TEST_SKIP is set")
    Nothing ->
      findExecutable "podman" >>= \case
        Nothing -> return (Left "podman is not on the PATH")
        Just _ ->
          podmanStatus 60 ["info", "--format", "{{.Host.Arch}}"] >>= \case
            (ExitSuccess, _, _) -> return (Right ())
            (_, _, err) -> return (Left ("podman info failed: " <> trim err))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
