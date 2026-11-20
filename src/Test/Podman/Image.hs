-- |
-- Module      : Test.Podman.Image
-- Copyright   : (c) 2026 JP Rupp
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The images that containers run: one of a registry, or one built from a
-- recipe that the tests carry.
--
-- = Pinned, so that a test means the same thing every time
--
-- A tag of a registry, such as @redis:7@, points at another image whenever
-- its owner pushes one, and a test that passed yesterday fails today with no
-- change of its own. Name an image of a registry by its digest, as in
-- @docker.io\/library\/redis:7-alpine\@sha256:…@, and it is the same image
-- for ever.
--
-- An image built here is tagged by a hash of its recipe and its files, as in
-- @localhost\/regtest:3f2a…@. A change of the recipe is a new tag, so a test
-- never runs an image built from an older recipe, and the old one is still
-- there for an older branch.
--
-- = Once, whatever runs at once
--
-- Eight test processes that start together would pull the same image eight
-- times, or build it eight times, each with a fresh layer cache. So pulling
-- and building take a lock of the machine for the image ("Test.Podman.Run"),
-- and look again whether the image is there once they have it: the first
-- process does the work, and the others find it done.
module Test.Podman.Image
  ( Image (..),
    ImageSpec (..),
    imageReference,
    ensureImage,
  )
where

import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Char (isAlphaNum, toLower)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import Test.Podman.Command (podmanStatus, podmanWithin)
import Test.Podman.Run (nextNumber, runFolder, withLock)

-- | The image of a container.
data Image
  = -- | an image of a registry, by its full reference, best pinned by its
    -- digest: @docker.io\/library\/redis:7-alpine\@sha256:…@. An image that
    -- the machine has already, such as one that a script built, is named
    -- the same way, as @localhost\/NAME:TAG@: nothing is pulled when Podman
    -- has the image.
    Registry !String
  | -- | an image built from a recipe
    Built !ImageSpec
  deriving (Eq, Show)

-- | The recipe of an image to build: what @podman build@ is given.
data ImageSpec = ImageSpec
  { -- | the name of the image, without a registry or a tag, such as
    -- @regtest@: it is built as @localhost\/NAME:HASH@
    name :: !String,
    -- | the Containerfile
    containerfile :: !ByteString,
    -- | the files of the context of the build, by their paths in it, for the
    -- @COPY@ and @ADD@ of the Containerfile
    files :: ![(FilePath, ByteString)]
  }
  deriving (Eq, Show)

-- | The reference by which Podman knows an image: the one given for an image
-- of a registry, and @localhost\/NAME:HASH@ for one to build, where the hash
-- is that of the Containerfile and every file of the context.
imageReference :: Image -> String
imageReference (Registry ref) = ref
imageReference (Built spec) = "localhost/" <> map clean spec.name <> ":" <> take 16 (hex digest)
  where
    clean c = if isAlphaNum c || c `elem` ("-_." :: String) then toLower c else '-'
    digest =
      SHA256.hash . B.concat $
        part spec.containerfile : concat [[part (C.pack path), part bytes] | (path, bytes) <- spec.files]
    -- each piece with its length, so that two recipes never hash the same by
    -- moving bytes from one piece to the next
    part bytes = C.pack (show (B.length bytes) <> ":") <> bytes
    hex = concatMap (\w -> let s = showHex w "" in if length s < 2 then '0' : s else s) . B.unpack

-- | Make sure that Podman has the image, pulling or building it if it does
-- not, and give its reference. One process of the machine does the work
-- while the others wait for it.
ensureImage :: Image -> IO String
ensureImage image = do
  let ref = imageReference image
  have <- exists ref
  unless have . withLock (lockName ref) $ do
    -- another process may have made it while this one waited
    again <- exists ref
    unless again $ case image of
      Registry _ -> () <$ podmanWithin 1800 ["pull", "--quiet", ref]
      Built spec -> build ref spec
  return ref
  where
    exists ref = (\(code, _, _) -> code == ExitSuccess) <$> podmanStatus 60 ["image", "exists", ref]
    lockName ref = "image-" <> take 32 (concatMap (\w -> showHex w "") (B.unpack (SHA256.hash (C.pack ref))))

-- | Build an image from its recipe, in a context folder of its own in the
-- folder of this run, which is removed afterwards.
build :: String -> ImageSpec -> IO ()
build ref spec = do
  n <- nextNumber
  dir <- (</> ("build-" <> show n)) <$> runFolder
  flip finally (removeDirectoryRecursive dir) $ do
    createDirectoryIfMissing True dir
    B.writeFile (dir </> "Containerfile") spec.containerfile
    forM_ spec.files $ \(path, bytes) -> do
      createDirectoryIfMissing True (takeDirectory (dir </> path))
      B.writeFile (dir </> path) bytes
    _ <- podmanWithin 1800 ["build", "--quiet", "--layers", "--tag", ref, "--file", dir </> "Containerfile", dir]
    return ()
