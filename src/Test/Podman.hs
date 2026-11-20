-- |
-- Module      : Test.Podman
-- Copyright   : (c) 2026 JP Rupp
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Podman containers for tests: the services that a test needs, such as a
-- database, a cache or a node, run in rootless containers for as long as
-- the test does, and are reached through Unix sockets.
--
-- = Purpose
--
-- A test of code that talks to a real service is the one that finds what a
-- mock cannot: the protocol as the service speaks it, its errors, its
-- timing. Running the service in a container gives every test the same
-- version of it, on every machine. Doing that well takes more than
-- @podman run@: the containers have to go away when the test does, even when
-- it fails; tests that run in parallel must not meet; and the test has to
-- wait until the service answers. This library does those things, for any
-- test framework: it is plain 'IO', in the shape of 'Control.Exception.bracket'.
--
-- = Design
--
-- * __Unix sockets, not ports.__ Every pod has a folder of its own on the
--   host, mounted into each of its containers at 'socketFolder', and every
--   endpoint of a service is a socket in it. Tests that run at once have
--   folders of their own, so they cannot meet, and no port of the machine is
--   taken. A service that can listen on a Unix socket does so ('Native'); one
--   that listens only on TCP is bridged to a socket by a small container of
--   @socat@ in its pod ('Bridged'). A pod whose endpoints are all sockets has
--   no network at all.
--
-- * __TCP where a client needs it.__ Some clients can only connect to a host
--   and a port. A 'Published' endpoint is published on a port of
--   @127.0.0.1@ that Podman picks, and its 'Address' says which.
--
-- * __Pods.__ The containers of a test are one pod: they share its network,
--   so they reach each other on @localhost@, and its folder of sockets.
--
-- * __Clean-up.__ A pod is removed when the action that uses it ends, however
--   it ends, and the pods of runs that were killed are removed by the next
--   run ("Test.Podman.Run").
--
-- * __Ready means ready.__ 'withPod' gives the pod to the action only once
--   every 'Readiness' of its spec holds: by default, once every endpoint
--   accepts connections.
--
-- * __Failures that explain themselves.__ When a pod does not become ready,
--   or the action fails, the last lines of the log of every container are
--   printed to the standard error.
--
-- * __Safe defaults.__ A container may use 512 MiB of memory unless its spec
--   says otherwise, and every call to Podman has a deadline.
--
-- = Example
--
-- A Redis that listens on a socket of the pod:
--
-- @
-- import Network.Socket (Socket)
-- import Test.Podman
--
-- redis :: PodSpec
-- redis =
--   pod
--     "redis"
--     [ (container "redis" (Registry "docker.io\/library\/redis:7-alpine"))
--         { command =
--             [ "redis-server", "--port", "0", "--save", ""
--             , "--unixsocket", socketInside "redis", "--unixsocketperm", "777" ]
--         , endpoints = [Native "redis"]
--         }
--     ]
--
-- main :: IO ()
-- main = withPod redis $ \\p -> do
--   s <- connectTo (address p "redis")
--   ...
-- @
--
-- = Requirements and limits
--
-- * Podman 4 or later, running rootless, on Linux. 'podmanUsable' says
--   whether it can run, so that a test can be skipped where it cannot.
-- * A 'Native' socket is made by the service, as the user that it runs as in
--   its container, which is not the user of the tests: the service must make
--   it open to everyone, as Redis does with @--unixsocketperm 777@. Bridged
--   sockets are made by the bridge, as the user of the tests.
-- * On macOS, Podman runs its containers in a virtual machine, whose shared
--   folders do not carry sockets: use 'Published' endpoints there.
module Test.Podman
  ( -- * Pods
    PodSpec (..),
    pod,
    withPod,
    withContainer,

    -- * Containers
    ContainerSpec (..),
    container,
    Endpoint (..),
    endpointName,
    socketFolder,
    socketInside,

    -- * Readiness
    Readiness (..),

    -- * A running pod
    Pod,
    address,
    podName,
    containerName,
    exec,
    execWithin,
    logs,

    -- * Addresses
    Address (..),
    sockAddr,
    connectTo,

    -- * Images
    Image (..),
    ImageSpec (..),
    bridgeImage,

    -- * Errors
    PodError (..),

    -- * Podman
    podmanUsable,
    podman,
    podmanWithin,
    PodmanError (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, bracket, bracketOnError, onException, throw, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Char (isAsciiLower, isDigit)
import Data.List (isInfixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTime)
import Network.Socket
  ( Family (..),
    SockAddr (..),
    Socket,
    SocketType (..),
    close,
    connect,
    defaultProtocol,
    socket,
    tupleToHostAddress,
  )
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (setFileMode)
import Test.Podman.Command (PodmanError (..), podman, podmanStatus, podmanUsable, podmanWithin)
import Test.Podman.Image (Image (..), ImageSpec (..), ensureImage)
import Test.Podman.Run (nextNumber, removeStale, runFolder, runLabel, runName)

-------------------------------------------------------------------------------
-- Specs
-------------------------------------------------------------------------------

-- | A pod to run: its containers, and when it is ready.
data PodSpec = PodSpec
  { -- | a short name, of lowercase letters, digits and dashes, which names
    -- the pod and its containers in @podman ps@
    name :: !String,
    -- | its containers, started in this order
    containers :: ![ContainerSpec],
    -- | whether its containers may reach the network beyond the pod. Without
    -- it, and without 'Published' endpoints, the pod has no network at all.
    outgoing :: !Bool,
    -- | what must hold before the pod is given to the action, checked in
    -- this order
    ready :: ![Readiness],
    -- | how many seconds all of 'ready' may take
    readyWithin :: !Int
  }

-- | A pod of these containers, of a name: ready once every endpoint of every
-- container accepts connections, within a minute, and without a network
-- beyond the pod.
pod :: String -> [ContainerSpec] -> PodSpec
pod name cs =
  PodSpec
    { name,
      containers = cs,
      outgoing = False,
      ready = [Accepts (endpointName e) | c <- cs, e <- c.endpoints],
      readyWithin = 60
    }

-- | A container of a pod.
data ContainerSpec = ContainerSpec
  { -- | a short name, unique in the pod, of lowercase letters, digits and
    -- dashes: the container is @POD-NAME@
    name :: !String,
    image :: !Image,
    -- | the entry point instead of that of the image, if any
    entrypoint :: !(Maybe String),
    -- | the command, which follows the entry point
    command :: ![String],
    -- | variables of its environment
    env :: ![(String, String)],
    -- | the most memory that it may use, as Podman reads it, such as
    -- @"512m"@, or no limit
    memory :: !(Maybe String),
    -- | folders in memory, each with its largest size, such as
    -- @("\/data", "256m")@
    tmpfs :: ![(FilePath, String)],
    -- | how the tests reach it
    endpoints :: ![Endpoint],
    -- | more arguments of @podman run@, put before the image
    options :: ![String]
  }
  deriving (Eq, Show)

-- | A container of a name and an image, with the command of the image, 512
-- MiB of memory and no endpoints.
container :: String -> Image -> ContainerSpec
container name image =
  ContainerSpec
    { name,
      image,
      entrypoint = Nothing,
      command = [],
      env = [],
      memory = Just "512m",
      tmpfs = [],
      endpoints = [],
      options = []
    }

-- | How the tests reach a service of a container. The name of an endpoint
-- is unique in its pod, and names its socket.
data Endpoint
  = -- | the service listens on the Unix socket 'socketInside' of this name
    Native !String
  | -- | the service listens on this TCP port, which a bridge joins to the
    -- Unix socket of this name
    Bridged !String !Int
  | -- | the service listens on this TCP port, which is published on a port
    -- of @127.0.0.1@ that Podman picks
    Published !String !Int
  deriving (Eq, Show)

-- | The name of an endpoint.
endpointName :: Endpoint -> String
endpointName (Native n) = n
endpointName (Bridged n _) = n
endpointName (Published n _) = n

-- | Where the folder of sockets of the pod is in each of its containers.
socketFolder :: FilePath
socketFolder = "/podman-test"

-- | The path, in a container, of the socket of an endpoint: what to give a
-- service that listens on a 'Native' socket.
socketInside :: String -> FilePath
socketInside n = socketFolder </> (n <> ".sock")

-- | What must hold before a pod is ready.
data Readiness
  = -- | the endpoint of this name accepts connections. For a socket of the
    -- service, a connection from the tests is accepted; for a TCP port, one
    -- from inside the pod, so that a bridge or a port forwarder that
    -- accepts before the service listens is not taken for the service.
    Accepts !String
  | -- | the log of this container has this text
    LogLine !String !String
  | -- | this command succeeds in this container, as with @podman exec@
    Succeeds !String ![String]
  | -- | this check succeeds, as it says
    Probe !String !(Pod -> IO Bool)

instance Show Readiness where
  show (Accepts e) = "endpoint " <> e <> " accepts connections"
  show (LogLine c t) = "the log of " <> c <> " shows " <> show t
  show (Succeeds c cmd) = unwords cmd <> " succeeds in " <> c
  show (Probe what _) = what

-- | The image of the bridges, pinned by its digest: socat on Alpine.
bridgeImage :: Image
bridgeImage =
  Registry
    "docker.io/alpine/socat:1.8.0.3\
    \@sha256:2d83bdac2858b4bcfa57d478ce53ae3c18a1147a68db4f610454a2d60e5c19bc"

-------------------------------------------------------------------------------
-- Running pods
-------------------------------------------------------------------------------

-- | A pod that runs.
data Pod = Pod
  { full :: !String,
    folder :: !FilePath,
    endpoints :: !(Map String (Endpoint, Address)),
    short :: ![String],
    bridge :: !(Maybe String)
  }

-- | What went wrong with a pod, other than a call to Podman.
data PodError
  = -- | the spec is not one that can run, for this reason
    BadSpec !String
  | -- | the pod of this name was not ready in time: this did not hold
    NotReady !String !String
  | -- | a container of the pod, of this name, stopped before it was ready
    Stopped !String
  | -- | the pod has no endpoint or container of this name
    NoSuch !String
  deriving (Eq)

instance Show PodError where
  show (BadSpec why) = "Bad pod spec: " <> why
  show (NotReady p what) = "Pod " <> p <> " was not ready in time: not " <> what
  show (Stopped c) = "Container " <> c <> " stopped before its pod was ready"
  show (NoSuch n) = "No endpoint or container " <> n <> " in the pod"

instance Exception PodError

-- | Run an action with a pod that runs and is ready, and remove the pod when
-- the action ends, however it ends.
--
-- The images are pulled or built if they are not there, which only the
-- first test of the machine waits for. Then the pod is made, its containers
-- started, a bridge started for its 'Bridged' endpoints, and every
-- 'Readiness' of its spec checked. If the pod is not ready in time, or the
-- action throws, the last lines of the log of each container go to the
-- standard error before the pod is removed.
withPod :: PodSpec -> (Pod -> IO a) -> IO a
withPod spec act = do
  either (throwIO . BadSpec) return (check spec)
  removeStale
  n <- nextNumber
  run <- runFolder
  let full = "pt-" <> runName <> "-" <> show n <> "-" <> spec.name
      dir = run </> show n
      eps = concatMap (.endpoints) spec.containers
      needsBridge = any (not . isNative) eps
  case [p | e <- eps, not (isPublished e), let p = dir </> (endpointName e <> ".sock"), length p > 107] of
    p : _ ->
      throwIO . BadSpec $
        "the socket " <> p <> " has a path longer than 107 bytes: use a shorter name, or PODMAN_TEST_DIR"
    [] -> return ()
  refs <- mapM (ensureImage . (.image)) spec.containers
  bridgeRef <- if needsBridge then Just <$> ensureImage bridgeImage else return Nothing
  bracket (create full dir eps) (const (remove full dir)) $ \_ -> do
    forM_ (zip spec.containers refs) $ \(c, ref) -> start full dir c ref
    forM_ bridgeRef $ \ref -> startBridge full dir eps ref
    addresses <- forM eps $ \e -> (,) (endpointName e) . (,) e <$> addressOf full dir e
    let p =
          Pod
            { full,
              folder = dir,
              endpoints = Map.fromList addresses,
              short = map (.name) spec.containers,
              bridge = full <> "-bridge" <$ bridgeRef
            }
    (waitReady spec p >> act p) `onException` printLogs p

-- | 'withPod' for a pod of one container, named after it.
withContainer :: ContainerSpec -> (Pod -> IO a) -> IO a
withContainer c = withPod (pod c.name [c])

-- | Why a spec cannot run, if it cannot.
check :: PodSpec -> Either String ()
check spec
  | not (goodName spec.name) = badName "pod" spec.name
  | null spec.containers = Left "a pod needs a container"
  | n : _ <- filter (not . goodName) names = badName "container" n
  | n : _ <- dups names = Left ("two containers are named " <> n)
  | "bridge" `elem` names = Left "the container name bridge is that of the bridge of the pod"
  | n : _ <- filter (not . goodName) eps = badName "endpoint" n
  | n : _ <- dups eps = Left ("two endpoints are named " <> n)
  | otherwise = Right ()
  where
    names = map (.name) spec.containers
    eps = [endpointName e | c <- spec.containers, e <- c.endpoints]
    goodName s = not (null s) && all (\c -> isAsciiLower c || isDigit c || c == '-') s
    badName what n = Left ("the " <> what <> " name " <> show n <> " is not lowercase letters, digits and dashes")
    dups xs = [x | x <- nub xs, length (filter (== x) xs) > 1]

isNative :: Endpoint -> Bool
isNative = \case
  Native _ -> True
  _ -> False

isPublished :: Endpoint -> Bool
isPublished = \case
  Published _ _ -> True
  _ -> False

-- | Make the folder of a pod and the pod. The folder is open to everyone, so
-- that a service that runs as another user than the tests can make its
-- socket in it; it is in the folder of the run, which only the user of the
-- tests can enter.
create :: String -> FilePath -> [Endpoint] -> IO ()
create full dir eps =
  bracketOnError (createDirectory dir >> setFileMode dir 0o777) (const (remove full dir)) $ \_ -> do
    let published = [p | Published _ p <- eps]
        network
          | null published = ["--network", "none"]
          | otherwise = concat [["--publish", "127.0.0.1::" <> show p] | p <- published]
    void . podman $ ["pod", "create", "--name", full, "--label", runLabel <> "=" <> runName] <> network

-- | Remove a pod with its containers, and its folder. Nothing that fails
-- here is an error: the next run removes what is left.
remove :: String -> FilePath -> IO ()
remove full dir = do
  void $ try @SomeException (podman ["pod", "rm", "--force", "--time", "0", full])
  void $ try @SomeException (removeDirectoryRecursive dir)

-- | The arguments of @podman run@ that every container of a pod has. A
-- stopped container stays until its pod goes, so that its log can be read.
runArgs :: String -> FilePath -> String -> [String]
runArgs full dir n =
  [ "run",
    "--detach",
    "--pod",
    full,
    "--name",
    full <> "-" <> n,
    "--label",
    runLabel <> "=" <> runName,
    -- every container of the pod shares the folder, so it is labelled for
    -- SELinux as shared (z), not as private to one container (Z)
    "--volume",
    dir <> ":" <> socketFolder <> ":z"
  ]

start :: String -> FilePath -> ContainerSpec -> String -> IO ()
start full dir c ref =
  void . podman $
    runArgs full dir c.name
      <> maybe [] (\m -> ["--memory", m]) c.memory
      <> concat [["--tmpfs", path <> ":rw,size=" <> size <> ",mode=1777"] | (path, size) <- c.tmpfs]
      <> concat [["--env", k <> "=" <> v] | (k, v) <- c.env]
      <> maybe [] (\e -> ["--entrypoint", e]) c.entrypoint
      <> c.options
      <> [ref]
      <> c.command

-- | Start the bridge of a pod: a socat for each 'Bridged' endpoint, which
-- makes the socket and joins each connection to it to the port of the
-- service. It runs as root of its container, which is the user of the tests
-- on the host, so the sockets are theirs. It stays even without a 'Bridged'
-- endpoint, to check 'Published' ones from inside the pod.
startBridge :: String -> FilePath -> [Endpoint] -> String -> IO ()
startBridge full dir eps ref =
  void . podman $
    runArgs full dir "bridge" <> ["--memory", "64m", "--entrypoint", "sh", ref, "-c", script]
  where
    script =
      concat
        [ "socat UNIX-LISTEN:" <> socketInside n <> ",fork,mode=777,unlink-early TCP:127.0.0.1:" <> show p <> " & "
        | Bridged n p <- eps
        ]
        <> "while :; do sleep 3600; done"

-- | The address of an endpoint of a pod that runs.
addressOf :: String -> FilePath -> Endpoint -> IO Address
addressOf full dir = \case
  Native n -> return (UnixSocket (dir </> (n <> ".sock")))
  Bridged n _ -> return (UnixSocket (dir </> (n <> ".sock")))
  Published _ p -> do
    out <- podman ["port", full <> "-" <> "bridge", show p <> "/tcp"]
    case reverse (takeWhile (/= ':') (reverse (headOr "" (lines out)))) of
      digits | not (null digits), all isDigit digits -> return (TcpPort "127.0.0.1" (read digits))
      _ -> throwIO (BadSpec ("Podman published port " <> show p <> " nowhere: " <> out))
  where
    headOr d = \case
      x : _ -> x
      [] -> d

-- | Wait until every 'Readiness' of a spec holds, in their order, and give
-- up at the deadline of the spec. A container that stops on the way stops
-- the wait at once.
waitReady :: PodSpec -> Pod -> IO ()
waitReady spec p = do
  deadline <- (+ fromIntegral spec.readyWithin) <$> getMonotonicTime
  forM_ spec.ready $ \r -> poll deadline r (0.05 :: Double)
  where
    poll deadline r pause = do
      ok <- holds p r
      unless ok $ do
        stopped p >>= mapM_ (throwIO . Stopped)
        now <- getMonotonicTime
        when (now > deadline) $ throwIO (NotReady p.full (show r))
        threadDelay (round (pause * 1000000))
        poll deadline r (min 0.5 (pause * 1.5))

-- | Whether a 'Readiness' holds now.
holds :: Pod -> Readiness -> IO Bool
holds p = \case
  Accepts n -> case Map.lookup n p.endpoints of
    Nothing -> throwIO (NoSuch n)
    Just (Native _, a) -> accepts a
    Just (Bridged _ port, a) -> (&&) <$> accepts a <*> inside port
    Just (Published _ port, _) -> inside port
  LogLine c text -> isInfixOf text <$> logs p c
  Succeeds c cmd -> do
    (code, _, _) <- podmanStatus 120 (["exec", containerName p c] <> cmd)
    return (code == ExitSuccess)
  Probe _ f -> f p
  where
    accepts a = either (const False) (const True) <$> try @SomeException (connectTo a >>= close)
    -- a TCP connection to the port, from the bridge, in the network of the pod
    inside port = case p.bridge of
      Nothing -> return False
      Just b -> do
        (code, _, _) <- podmanStatus 120 ["exec", b, "socat", "-u", "OPEN:/dev/null", "TCP:127.0.0.1:" <> show port]
        return (code == ExitSuccess)

-- | The containers of a pod that have stopped, if any.
stopped :: Pod -> IO [String]
stopped p = do
  out <- podman ["ps", "--all", "--filter", "pod=" <> p.full, "--format", "{{.Names}} {{.State}}"]
  return [n | [n, st] <- map words (lines out), st `elem` ["exited", "stopped", "dead"]]

-- | Print the last lines of the log of every container of a pod.
printLogs :: Pod -> IO ()
printLogs p = forM_ (p.short <> ["bridge" | Just _ <- [p.bridge]]) $ \c -> do
  r <- try @SomeException (podmanStatus 60 ["logs", "--tail", "40", containerName p c])
  case r of
    Right (_, out, err) -> hPutStrLn stderr ("--- the log of " <> containerName p c <> ":\n" <> out <> err)
    Left _ -> return ()

-- | The address of an endpoint of a pod, by its name. Throws 'NoSuch' if the
-- pod has no endpoint of the name.
address :: Pod -> String -> Address
address p n = maybe (throw (NoSuch n)) snd (Map.lookup n p.endpoints)

-- | The name of the pod, as @podman pod ps@ shows it.
podName :: Pod -> String
podName p = p.full

-- | The name of a container of the pod, as @podman ps@ shows it, from its
-- name in the spec.
containerName :: Pod -> String -> String
containerName p c = p.full <> "-" <> c

-- | Run a command in a container of the pod, as @podman exec@ does, and give
-- what it printed. Throws a t'PodmanError' if it fails or takes more than two
-- minutes.
exec :: Pod -> String -> [String] -> IO String
exec = execWithin 120

-- | 'exec' with a deadline of so many seconds.
execWithin :: Int -> Pod -> String -> [String] -> IO String
execWithin secs p c cmd = podmanWithin secs (["exec", containerName p c] <> cmd)

-- | The log of a container of the pod, its standard output and then its
-- standard error.
logs :: Pod -> String -> IO String
logs p c = (\(_, out, err) -> out <> err) <$> podmanStatus 60 ["logs", containerName p c]

-------------------------------------------------------------------------------
-- Addresses
-------------------------------------------------------------------------------

-- | Where the tests reach an endpoint.
data Address
  = -- | a Unix socket at this path on the host
    UnixSocket !FilePath
  | -- | a TCP port of this host, which is always @127.0.0.1@
    TcpPort !String !Int
  deriving (Eq, Ord, Show)

-- | The address as the @network@ package takes it.
sockAddr :: Address -> SockAddr
sockAddr (UnixSocket path) = SockAddrUnix path
sockAddr (TcpPort _ port) = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

-- | A socket connected to an address. The caller closes it.
connectTo :: Address -> IO Socket
connectTo a = bracketOnError (socket family Stream defaultProtocol) close $ \s -> s <$ connect s (sockAddr a)
  where
    family = case a of
      UnixSocket _ -> AF_UNIX
      TcpPort _ _ -> AF_INET
