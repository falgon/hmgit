{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module HMGit.Core (
    storeObject
  , loadObject
) where

import           Codec.Compression.Zlib    (compress, decompress)
import           Control.Exception.Safe    (MonadThrow (..), SomeException,
                                            throwString)
import           Control.Monad             (MonadPlus (..))
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Crypto.Hash.SHA1          (hashlazy)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8      as BU
import           Data.List                 (intercalate, isPrefixOf, unwords,
                                            words)
import qualified Data.List.NonEmpty        as LN
import           Data.Void                 (Void)
import           Data.Word                 (Word8)
import           Prelude                   hiding (init)
import           System.Directory          (createDirectoryIfMissing,
                                            listDirectory)
import           System.FilePath           ((</>))
import qualified Text.Megaparsec           as M
import qualified Text.Megaparsec.Error     as M

import           HMGit.Parser

hmGitDir :: FilePath -> FilePath
hmGitDir = flip (</>) ".hmgit"

hmGitInitDir :: FilePath -> [FilePath]
hmGitInitDir repoName = map (hmGitDir repoName </>) [
    "objects"
  , "refs" </> "heads"
  ]

init :: FilePath -> IO ()
init repoName = mapM_ (createDirectoryIfMissing True) (hmGitInitDir repoName)
    *> BL.writeFile (hmGitDir repoName </> "HEAD") "ref: refs/heads/master"

hashByteData :: BL.ByteString -> ObjectType -> BL.ByteString
hashByteData rawData objType = mconcat [
    BLU.fromString $ show objType
  , " "
  , BLU.fromString $ show $ BL.length rawData
  , BL.singleton 0
  , rawData
  ]

hashObject :: BL.ByteString -> ObjectType -> B.ByteString
hashObject = (.) hashlazy . hashByteData

hashToPath :: B.ByteString -> (FilePath, FilePath)
hashToPath sha1 = (
    intercalate "/" [ hmGitDir ".", "objects", BU.toString $ B.take 2 sha1 ]
  , BU.toString $ B.drop 2 sha1
  )

storeObject :: ObjectType -> BL.ByteString -> IO B.ByteString
storeObject objType rawData = sha1 <$
    (createDirectoryIfMissing True (fst path) *> BL.writeFile (uncurry (</>) path) (compress fullData))
    where
        fullData = hashByteData rawData objType
        sha1 = hashlazy fullData
        path = hashToPath sha1

loadObject :: MonadThrow m => B.ByteString -> IO (m (ObjectType, BL.ByteString))
loadObject sha1
    | B.length sha1 < 1 = pure $ throwString "hash prefix must be 2 or more characters"
    | otherwise = runMaybeT getValidObject >>= \case
        Nothing -> throwString $ unwords [
            "objects"
          , BU.toString sha1
          , "not found or multiple object ("
          , show $ B.length sha1
          , ") with prefix"
          , BU.toString sha1
          ]
        Just (fname, object) -> case M.runParser objectParser fname object of
            Left errorBundle -> throwString $ M.errorBundlePretty errorBundle
            Right (objType, body) -> pure $ pure (objType, body)
    where
        getValidObject = do
            let (dir, rest) = hashToPath sha1
            (object, ext) <- LN.splitAt 1 <$> MaybeT (LN.nonEmpty . filter (isPrefixOf rest) <$> listDirectory dir)
            if not $ null ext then mzero else let fname = dir </> head object in
                (fname,) <$> lift (decompress <$> BL.readFile fname)
