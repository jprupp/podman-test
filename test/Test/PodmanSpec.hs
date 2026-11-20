-- |
-- Module      : Test.PodmanSpec
-- Copyright   : (c) 2026 JP Rupp
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Tests of the library against Podman: Redis, pinned by its digest, in pods
-- reached through a socket of its own, through a bridge and through a port;
-- a pod that needs no bridge and runs none;
-- two containers of a pod that talk on @localhost@; eight pods at once;
-- containers that stop, pods that are never ready, stale runs, specs that
-- cannot run, and images built by several processes at once.
--
-- The tests that need Podman are pending where 'podmanUsable' says that it
-- cannot run. Each group at the top is one thing, so that a tool that runs
-- the groups in parallel processes, such as haskell-dev, may do so.
module Test.PodmanSpec (spec) where

import Control.Concurrent.Async (mapConcurrently, replicateConcurrently)
import Control.Exception (bracket, try)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.List (isInfixOf, isPrefixOf, nub)
import Network.Socket (close)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (fileOwner, getFileStatus)
import System.Posix.User (getEffectiveUserID)
import Test.Hspec
import Test.Podman
import Test.Podman.Command (podmanStatus)
import Test.Podman.Image (imageReference)
import Test.Podman.Run (baseFolder, removeStaleRuns, runFolder, runLabel)

-- | Redis, pinned by its digest.
redisImage :: Image
redisImage =
  Registry
    "docker.io/library/redis:7-alpine\
    \@sha256:ec1c2d3603c3d7d65b153bedda0248b641aa3bd7684503406b27b9e870ac0796"

-- | A Redis that listens on a TCP port, and on nothing else.
redisTcp :: ContainerSpec
redisTcp =
  (container "redis" redisImage)
    { command = ["redis-server", "--port", "6379", "--save", ""],
      memory = Just "128m"
    }

-- | A Redis that listens on a socket of its pod, open to everyone.
redisNative :: ContainerSpec
redisNative =
  redisTcp
    { command =
        [ "redis-server",
          "--port",
          "0",
          "--save",
          "",
          "--unixsocket",
          socketInside "redis",
          "--unixsocketperm",
          "777"
        ],
      endpoints = [Native "redis"]
    }

-- | What Redis answers to PING at an address.
ping :: Address -> IO String
ping a = bracket (connectTo a) close $ \s -> do
  sendAll s (C.pack "PING\r\n")
  C.unpack <$> recv s 64

-- | Whether Podman has a pod of this name.
podExists :: String -> IO Bool
podExists n = (\(code, _, _) -> code == ExitSuccess) <$> podmanStatus 60 ["pod", "exists", n]

-- | Whether Podman has a container of this name.
containerExists :: String -> IO Bool
containerExists n = (\(code, _, _) -> code == ExitSuccess) <$> podmanStatus 60 ["container", "exists", n]

spec :: Spec
spec = do
  usable <- runIO podmanUsable
  let podmanIt name test = it name $ either pendingWith (const test) usable

  describe "endpoints" $ do
    podmanIt "reaches a service on its own socket, and removes the pod and its folder after" $ do
      (name, dir) <- withContainer redisNative $ \p -> do
        let a = address p "redis"
        ping a `shouldReturn` "+PONG\r\n"
        case a of
          UnixSocket path -> do
            base <- baseFolder
            path `shouldSatisfy` (base `isPrefixOf`)
            return (podName p, takeFolder path)
          TcpPort _ _ -> fail "Not a socket"
      podExists name `shouldReturn` False
      doesDirectoryExist dir `shouldReturn` False

    podmanIt "bridges a TCP port to a socket of the user of the tests" $
      withContainer redisTcp {endpoints = [Bridged "redis" 6379]} $ \p -> do
        let a = address p "redis"
        ping a `shouldReturn` "+PONG\r\n"
        case a of
          UnixSocket path -> do
            me <- getEffectiveUserID
            fileOwner <$> getFileStatus path `shouldReturn` me
          TcpPort _ _ -> expectationFailure "Not a socket"

    podmanIt "publishes a TCP port on the loopback interface" $
      withContainer redisTcp {endpoints = [Published "redis" 6379]} $ \p -> do
        let a = address p "redis"
        case a of
          TcpPort host port -> do
            host `shouldBe` "127.0.0.1"
            port `shouldSatisfy` (> 0)
          UnixSocket _ -> expectationFailure "Not a port"
        ping a `shouldReturn` "+PONG\r\n"

    podmanIt "runs no bridge in a pod of published ports that is ready by a command of its own" $ do
      let published = redisTcp {endpoints = [Published "redis" 6379]}
          byCommand = (pod "plain" [published]) {ready = [Succeeds "redis" ["redis-cli", "ping"]]}
      withPod byCommand $ \p -> do
        containerExists (containerName p "redis") `shouldReturn` True
        containerExists (containerName p "bridge") `shouldReturn` False
        ping (address p "redis") `shouldReturn` "+PONG\r\n"

  describe "pods" $ do
    podmanIt "lets the containers of a pod reach each other on localhost" $ do
      let client =
            (container "client" redisImage)
              { entrypoint = Just "sh",
                command = ["-c", "while :; do sleep 3600; done"],
                memory = Just "64m"
              }
          spec' =
            (pod "pair" [redisTcp, client])
              { ready = [Succeeds "client" ["redis-cli", "-h", "127.0.0.1", "ping"]]
              }
      withPod spec' $ \p ->
        exec p "client" ["redis-cli", "-h", "127.0.0.1", "ping"] `shouldReturn` "PONG"

    podmanIt "runs eight pods at once, each with its own socket" $ do
      answers <- replicateConcurrently 8 . withContainer redisNative $ \p -> do
        let a = address p "redis"
        (,) a <$> ping a
      map snd answers `shouldBe` replicate 8 "+PONG\r\n"
      length (nub (map fst answers)) `shouldBe` 8

  describe "failures" $ do
    podmanIt "stops waiting when a container stops, and removes the pod" $ do
      r <-
        try . withContainer redisNative {command = [], entrypoint = Just "false"} $
          \p -> return (podName p)
      case r of
        Left (Stopped c) -> c `shouldSatisfy` ("-redis" `isSuffixOf'`)
        Left e -> expectationFailure ("Another error: " <> show e)
        Right _ -> expectationFailure "The pod was ready"

    podmanIt "gives up on a pod that is not ready in time" $ do
      let never = (pod "never" [redisTcp]) {ready = [LogLine "redis" "no such line"], readyWithin = 2}
      withPod never (\_ -> return ()) `shouldThrow` \case
        NotReady _ what -> "no such line" `isInfixOf` what
        _ -> False

    it "refuses a spec that cannot run" $ do
      let bad = pod "bad" [redisNative, redisNative]
      withPod bad (\_ -> return ()) `shouldThrow` \case
        BadSpec _ -> True
        _ -> False
      withPod (pod "Bad Name" [redisNative]) (\_ -> return ()) `shouldThrow` \case
        BadSpec _ -> True
        _ -> False

    podmanIt "refuses a socket whose path is too long for Linux" $ do
      let long = redisNative {endpoints = [Native (replicate 100 'a')]}
      withContainer long (\_ -> return ()) `shouldThrow` \case
        BadSpec _ -> True
        _ -> False

  describe "stale runs" $
    podmanIt "removes the pods and folders of runs that died, and nothing of one that runs" $ do
      let dead = "999999999-1"
          stale = "pt-" <> dead <> "-1-stale"
      void $ podmanStatus 60 ["pod", "rm", "--force", stale]
      void $ podmanStatus 60 ["pod", "create", "--name", stale, "--label", runLabel <> "=" <> dead]
      base <- baseFolder
      createDirectoryIfMissing True (base </> dead </> "1")
      mine <- runFolder
      removeStaleRuns
      podExists stale `shouldReturn` False
      doesPathExist (base </> dead) `shouldReturn` False
      doesDirectoryExist mine `shouldReturn` True

  describe "images" $ do
    it "names a built image by the hash of its recipe" $ do
      let recipe = ImageSpec {name = "Echo Test", containerfile = C.pack "FROM scratch\n", files = []}
          ref = imageReference (Built recipe)
      ref `shouldSatisfy` ("localhost/echo-test:" `isPrefixOf`)
      imageReference (Built recipe {files = [("a", C.pack "b")]}) `shouldNotBe` ref
      imageReference (Built recipe) `shouldBe` ref

    podmanIt "builds an image once for several builds at once, and runs it" $ do
      let base = case redisImage of
            Registry r -> r
            Built _ -> error "not a registry image"
          recipe =
            ImageSpec
              { name = "podman-test-spec",
                containerfile = C.pack ("FROM " <> base <> "\nCOPY hello.txt /hello.txt\n"),
                files = [("hello.txt", C.pack "hello")]
              }
          image = Built recipe
      void $ podmanStatus 60 ["image", "rm", "--force", imageReference image]
      let reader =
            (container "reader" image)
              { entrypoint = Just "sh",
                command = ["-c", "while :; do sleep 3600; done"],
                memory = Just "64m"
              }
      let readIt _ = withContainer reader (\p -> exec p "reader" ["cat", "/hello.txt"])
      outs <- mapConcurrently readIt [1 .. 3 :: Int]
      outs `shouldBe` replicate 3 "hello"

  describe "podmanUsable" $
    it "says why it cannot run when PODMAN_TEST_SKIP is set" $
      withSkip $
        podmanUsable `shouldReturn` Left "PODMAN_TEST_SKIP is set"
  where
    takeFolder = reverse . drop 1 . dropWhile (/= '/') . reverse
    isSuffixOf' s x = reverse s `isPrefixOf` reverse x
    withSkip act = bracket (setEnv "PODMAN_TEST_SKIP" "1") (const (unsetEnv "PODMAN_TEST_SKIP")) (const act)
