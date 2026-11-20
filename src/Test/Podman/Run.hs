-- |
-- Module      : Test.Podman.Run
-- Copyright   : (c) 2026 JP Rupp
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- What sets one run of the tests apart from every other: its name, its
-- folder of sockets, the names of its containers, and the clean-up of what
-- runs that died left behind.
--
-- = A run
--
-- A run is one process: a test binary, or one of the processes that a tool
-- such as haskell-dev starts to run the tests in parallel. Its name is its
-- process ID and the time at which the process started, as the kernel keeps
-- it in @\/proc@. The process ID alone is not enough: the kernel gives the
-- numbers out again, and a new process with the number of a dead run would
-- keep that run's containers alive for ever.
--
-- Every pod and container of a run carries the label @podman-test.run@ with
-- its name, and every pod has a folder under the folder of the run.
--
-- = Clean-up without a reaper
--
-- A run removes its containers when the tests that use them end, even when
-- they fail. A run that is killed cannot, so the first pod of every run looks
-- for pods and folders of runs whose process is gone, and removes them. A
-- live run is never touched: its process exists, with the same start time.
-- There is no reaper container to start, as the Docker world uses: the next
-- run is the reaper.
--
-- = The folder
--
-- Unix sockets have a short limit on the length of their path, 107 bytes on
-- Linux, so the folder has to be short: @$XDG_RUNTIME_DIR\/podman-test@,
-- which is in memory and private to the user, or @\/tmp\/podman-test-UID@
-- where there is no runtime folder. @PODMAN_TEST_DIR@ chooses another.
--
-- = Private to the user
--
-- The folder of each pod is open to everyone, so that a service that runs
-- as another user in its container can make its socket there, and such a
-- socket is open to everyone, so that the tests can connect to it. What
-- keeps other users of the machine out is the folder above them all, which
-- must be the user's alone ('privateFolder'): made with no access for
-- anyone else, and refused if it is not a folder of the user, as when
-- another user made @\/tmp\/podman-test-UID@ first, or put a link to a
-- folder of theirs there. A folder of the user that others may read is
-- closed to them. Without that, another user could reach the services of
-- the tests, which have no password, or put files in the folders of their
-- sockets.
module Test.Podman.Run
  ( -- * The run
    runName,
    runFolder,
    baseFolder,
    privateBase,
    privateFolder,
    UnsafeFolder (..),
    nextNumber,
    runLabel,

    -- * Clean-up
    removeStale,
    removeStaleRuns,

    -- * Locks
    withLock,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (Exception, SomeException, bracket, catch, evaluate, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.))
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import GHC.IO.Handle.Lock (LockMode (..), hLock)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hClose, openFile)
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Directory (createDirectory)
import System.Posix.Files
  ( fileMode,
    fileOwner,
    getSymbolicLinkStatus,
    isDirectory,
    isSymbolicLink,
    setFileMode,
  )
import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserID)
import Test.Podman.Command (podman)

-- | The label that every pod and container of a run carries, with the name
-- of the run as its value.
runLabel :: String
runLabel = "podman-test.run"

-- | The name of this run: its process ID and the start time of its process,
-- joined by a dash, as in @4242-918273@.
runName :: String
runName = unsafePerformIO $ do
  pid <- show <$> getProcessID
  started <- maybe "0" id <$> startTime pid
  return (pid <> "-" <> started)
{-# NOINLINE runName #-}

-- | The start time of a process, in the clock ticks since the boot of the
-- machine that the kernel counts, if the process exists and the system has
-- @\/proc@.
startTime :: String -> IO (Maybe String)
startTime pid = do
  r <- try (readFile ("/proc" </> pid </> "stat") >>= evaluate . fieldOf)
  return $ case r of
    Left (_ :: SomeException) -> Nothing
    Right v -> v
  where
    -- the name of the program, the second field, may hold spaces and
    -- brackets, so the fields are counted from after its last bracket: the
    -- start time is the 22nd field, the 20th after the name
    fieldOf s = case drop 19 (words (afterName s)) of
      t : _ | all isDigit t -> Just t
      _ -> Nothing
    afterName s = case break (== ')') (reverse s) of
      (rest, _ : _) -> reverse rest
      _ -> s

-- | Whether the run of this name is still going: its process exists and
-- started when the name says. Where there is no @\/proc@, every run is taken
-- to be going, so that nothing of a live run is ever removed.
alive :: String -> IO Bool
alive name = case break (== '-') name of
  (pid, '-' : started)
    | not (null pid),
      all isDigit pid -> do
        proc <- doesDirectoryExist "/proc/self"
        if not proc then return True else (== Just started) <$> startTime pid
  _ -> return True

-- | The folder under which every run has its own: @PODMAN_TEST_DIR@ if it is
-- set, @$XDG_RUNTIME_DIR\/podman-test@ if there is a runtime folder, and
-- @\/tmp\/podman-test-UID@ otherwise.
baseFolder :: IO FilePath
baseFolder =
  lookupEnv "PODMAN_TEST_DIR" >>= \case
    Just dir | not (null dir) -> return dir
    _ ->
      lookupEnv "XDG_RUNTIME_DIR" >>= \case
        Just dir | not (null dir) -> return (dir </> "podman-test")
        _ -> ("/tmp/podman-test-" <>) . show <$> getEffectiveUserID

-- | 'baseFolder', made private to the user ('privateFolder').
privateBase :: IO FilePath
privateBase = do
  dir <- baseFolder
  privateFolder dir
  return dir

-- | A folder that cannot be made private to the user, and why.
data UnsafeFolder = UnsafeFolder !FilePath !String
  deriving (Eq)

instance Show UnsafeFolder where
  show (UnsafeFolder path why) =
    "The folder "
      <> path
      <> " of podman-test is not private to this user: "
      <> why
      <> ". Remove it, or choose another with PODMAN_TEST_DIR."

instance Exception UnsafeFolder

-- | Make sure that a folder is the user's alone: make it, with its parents,
-- with no access for anyone else if it is not there; take away the access
-- of others if it is a folder of the user's that others may use; and throw
-- t'UnsafeFolder' if it is a link, not a folder, or a folder of another
-- user, none of which the user can trust.
privateFolder :: FilePath -> IO ()
privateFolder dir = do
  createDirectoryIfMissing True (takeDirectory dir)
  made <- try (createDirectory dir 0o700)
  case made of
    Left e | not (isAlreadyExistsError e) -> ioError e
    _ -> return ()
  st <- getSymbolicLinkStatus dir
  me <- getEffectiveUserID
  when (isSymbolicLink st) $ throwIO (UnsafeFolder dir "it is a symbolic link")
  unless (isDirectory st) $ throwIO (UnsafeFolder dir "it is not a folder")
  when (fileOwner st /= me) $
    throwIO (UnsafeFolder dir ("it belongs to another user, of UID " <> show (fileOwner st)))
  when (fileMode st .&. 0o077 /= 0) $ setFileMode dir 0o700

-- | The folder of this run, made if it is not there, in the private base
-- folder.
runFolder :: IO FilePath
runFolder = do
  dir <- (</> runName) <$> privateBase
  createDirectoryIfMissing False dir
  return dir

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

-- | A number that no other call in this run gets, from 1 up: what keeps the
-- names of two pods of the same run apart.
nextNumber :: IO Int
nextNumber = atomicModifyIORef' counter (\n -> (n + 1, n + 1))

cleaned :: IORef Bool
cleaned = unsafePerformIO (newIORef False)
{-# NOINLINE cleaned #-}

-- | 'removeStaleRuns' once in a run: the first call does it, and later
-- calls do nothing. 'Test.Podman.withPod' calls it.
removeStale :: IO ()
removeStale = do
  first <- atomicModifyIORef' cleaned (\done -> (True, not done))
  when first removeStaleRuns

-- | Remove the pods, with their containers, and the folders of runs whose
-- process is gone. Nothing of a run that is going is ever removed, and a
-- failure to remove something is not an error: a run that cannot clean up
-- still runs.
removeStaleRuns :: IO ()
removeStaleRuns = do
  -- the names of the pods of every run, and the run of each from the label:
  -- podman pod ps writes labels differently from one version of Podman to
  -- the next, but podman pod inspect keeps them in a map in every one
  pods <-
    lines
      <$> podman ["pod", "ps", "--filter", "label=" <> runLabel, "--format", "{{.Name}}"]
        `catch` \(_ :: SomeException) -> return []
  forM_ pods $ \pod -> do
    let labelOf = "{{index .Labels \"" <> runLabel <> "\"}}"
    label <- try @SomeException (podman ["pod", "inspect", pod, "--format", labelOf])
    case label of
      Right name | not (null name) -> do
        going <- alive name
        unless going . void $
          try @SomeException (podman ["pod", "rm", "--force", "--time", "0", pod])
      _ -> return ()
  base <- privateBase
  exists <- doesDirectoryExist base
  when exists $ do
    names <- listDirectory base
    forM_ [n | n <- names, n /= "locks"] $ \name -> do
      going <- alive name
      unless going . void $ try @SomeException (removeDirectoryRecursive (base </> name))

-- | Run an action while holding the lock of this name, which every run of
-- the machine shares: for what two runs must not do at once, such as
-- building the same image. The lock is a file under the base folder, locked
-- with @flock@, which the kernel lets go of if the process dies.
--
-- The threads of one process take turns at it first, with a lock of the
-- process: GHC does not let a process open one file for writing twice, so
-- the second thread would fail to open the file instead of waiting.
withLock :: String -> IO a -> IO a
withLock name act = do
  fresh <- newMVar ()
  turn <- atomicModifyIORef' locks $ \m -> case Map.lookup name m of
    Just old -> (m, old)
    Nothing -> (Map.insert name fresh m, fresh)
  withMVar turn $ \_ -> do
    dir <- (</> "locks") <$> privateBase
    createDirectoryIfMissing False dir
    bracket (openFile (dir </> name) ReadWriteMode) hClose $ \h -> do
      hLock h ExclusiveLock
      act

locks :: IORef (Map.Map String (MVar ()))
locks = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE locks #-}
