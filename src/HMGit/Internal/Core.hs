{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, TemplateHaskell,
             TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module HMGit.Internal.Core (
    ObjectType (..)
  , ObjectInfo (objectId, objectData, objectPath)
  , IndexEntry (..)
  , fromContents
  , storeObject
  , loadObject
  , loadTree
  , storeTree
  , loadIndex
  , storeIndex
  , HMGitStatus (statusChanged, statusNew, statusDeleted)
  , latestBlobHashes
  , indexedBlobHashes
  , getStatus
) where

import           HMGit.Internal.Core.Runner
import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser      (IndexEntry (..), ObjectType (..),
                                             indexParser, objectParser,
                                             putIndex, runByteStringParser,
                                             treeParser)
import           HMGit.Internal.Utils       (hexStr, strictOne)

import           Codec.Compression.Zlib     (compress, decompress)
import           Control.Exception.Safe     (MonadCatch, MonadThrow, catch,
                                             catchAny, throw)
import           Control.Monad              (MonadPlus, filterM)
import           Control.Monad.Extra        (ifM)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans        (lift)
import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.Binary.Put            as BP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.UTF8       as BU
import           Data.Functor               (($>), (<&>))
import           Data.List                  (isPrefixOf)
import qualified Data.Map.Lazy              as ML
import qualified Data.Set                   as S
import           Data.String                (IsString (..))
import           Data.Tuple.Extra           (dupe, first, firstM, second)
import           Path                       (Dir, File, Rel)
import qualified Path                       as P
import qualified Path.IO                    as P
import           Prelude                    hiding (init)
import           System.IO                  (hPutStrLn, stderr)
import           System.Posix.Types         (CMode (..))
import           Text.Printf                (printf)

hmGitObjectsDirLength :: Int
hmGitObjectsDirLength = 2

-- | Information for built objects
data ObjectInfo = ObjectInfo {
    objectId   :: BU.ByteString -- ^ Object ID, SHA1 hash value
  , objectData :: BL.ByteString -- ^ Object data, compressed by zlib
  , objectPath :: P.Path P.Abs P.File -- ^ Object relative file path based on root of working tree
  }

formatObject :: ObjectType
    -> BL.ByteString
    -> BL.ByteString
formatObject objType contents = BP.runPut $
    BP.putByteString (fromString $ show objType)
        *> BP.putWord8 32 -- Space
        *> BP.putByteString (fromString $ show $ BL.length contents)
        *> BP.putWord8 0
        *> BP.putLazyByteString contents

hashToObjectPath :: MonadCatch m
    => String
    -> HMGitT m (Either (P.Path P.Abs P.Dir) (P.Path P.Abs P.File))
hashToObjectPath hexSha1
    | length hexSha1 < hmGitObjectsDirLength = lift
        $ throw
        $ invalidArgument
        $ printf "hash prefix must be %d or more characters"
            hmGitObjectsDirLength
    | otherwise = do
        (dir, fname) <- firstM P.parseRelDir
            $ splitAt hmGitObjectsDirLength hexSha1
        ((\x y -> Right $ x P.</> $(P.mkRelDir "objects") P.</> dir P.</> y)
            <$> hmGitDBPath
            <*> lift (P.parseRelFile fname))
            `catch` \e@(P.InvalidRelFile fp) -> if null fp
                then hmGitDBPath <&> Left . (P.</> ($(P.mkRelDir "objects") P.</> dir))
                else lift $ throw e

-- | Create object ID, path, zlib compressed data from object type and content
fromContents :: MonadCatch m
    => ObjectType
    -> BL.ByteString
    -> HMGitT m ObjectInfo
fromContents objType contents = hashToObjectPath (hexStr objId)
    >>= either
        (const $ throw $ BugException "fromContents: hashToObjectPath must give the Abs file")
        (pure . ObjectInfo objId (compress objFormat))
    where
        objFormat = formatObject objType contents
        objId = hashlazy objFormat

-- | Create an object to HMGit database
storeObject :: (MonadIO m, MonadCatch m)
    => ObjectType
    -> BL.ByteString
    -> HMGitT m B.ByteString
storeObject objType contents = do
    objInfo <- fromContents objType contents
    P.createDirIfMissing True (P.parent $ objectPath objInfo)
        *> liftIO (BL.writeFile (P.toFilePath $ objectPath objInfo) $ objectData objInfo)
        $> objectId objInfo

-- | Read an object from HMGit database by Object ID
loadObject :: (MonadIO m, MonadCatch m, MonadPlus m)
    => String
    -> HMGitT m (ObjectType, BL.ByteString)
loadObject sha1 = do
    fname <- hashToObjectPath sha1
        >>= uncurry findTarget . either (,mempty) (first P.parent . second (P.toFilePath . P.filename) . dupe)
    liftIO (BL.readFile $ P.toFilePath fname)
        >>= runByteStringParser objectParser fname . decompress
    where
        findTarget dir fname = catchAny (findTargetObject dir fname) $ const
            $ lift
            $ throw
            $ noSuchThing
                (printf "objects %s not found or multiple object (%d) with prefix %s"
                    sha1 (length sha1) sha1)
                (P.toFilePath dir <> "/" <> fname)

        findTargetObject dir fname = P.listDirRel dir
            >>= strictOne . filter (isPrefixOf fname . P.toFilePath) . snd
            <&> (dir P.</>)

-- | Read a tree from raw data
loadTree :: MonadThrow m
    => BL.ByteString
    -> HMGitT m [(CMode, P.Path P.Rel P.File, String)]
loadTree body = hmGitTreeLim
    >>= flip (`runByteStringParser` $(P.mkRelFile "index")) body
     . treeParser

-- | Create a tree object to HMGit database
-- __NOTE__: Currently only supports a single, top-level directory
storeTree :: (MonadIO m, MonadCatch m) => HMGitT m B.ByteString
storeTree = loadIndex
    >>= filterM predUnsupported
    >>= storeObject Tree . foldMap (BP.runPut . putter)
    where
        predUnsupported e
            | '/' `elem` P.toFilePath (iePath e) =
                liftIO $ hPutStrLn stderr
                    (printf "warning: sorry, currently only supports a single, top-level directory, so %s is ignored."
                        (P.toFilePath $ iePath e)) $> False
            | otherwise = pure True
        putter e = BP.putLazyByteString (fromString (printf "%o %s\0" (ieMode e) (P.toFilePath $ iePath e)))
            *> BP.putLazyByteString (ieSha1 e)

-- | Read index entries
loadIndex :: (MonadIO m, MonadThrow m) => HMGitT m [IndexEntry]
loadIndex = do
    fname <- hmGitIndexPath
    ifM (not <$> P.doesFileExist fname) (pure []) $
        liftIO (BL.readFile $ P.toFilePath fname)
            >>= runByteStringParser indexParser fname

-- | Write index entries
storeIndex :: (MonadIO m, Foldable t)
    => t IndexEntry
    -> HMGitT m ()
storeIndex es = hmGitIndexPath
    >>= liftIO . flip B.writeFile (idxData <> digest) . P.toFilePath
    where
        digest = hashlazy $ BL.fromStrict idxData
        idxData = BL.toStrict $ BP.runPut $ putIndex es

-- | Calculates the latest, or current,
-- hash of the working tree and returns a map of its path and hash value.
--
-- __NOTE__: Currently `latestBlobHashes` does not support gitignore and submodule,
-- so we are embedding content to ignore directly in the code.
-- Comments HACK below are the relevant part.
latestBlobHashes :: (MonadIO m, MonadCatch m)
    => HMGitT m (ML.Map (P.Path P.Rel P.File) String)
latestBlobHashes = hmGitRoot
    >>= P.walkDirAccumRel (Just dirPred) dirAccum
    <&> ML.fromList
    where
        dirPred d _ _
            | $(P.mkRelDir "./") == d = do
                dbDir <- hmGitDBName >>= P.parseRelDir
                pure $ P.WalkExclude [
                    dbDir
                  , $(P.mkRelDir ".stack-work") -- HACK
                  ]
            | $(P.mkRelDir "test") == d = pure $ P.WalkExclude [ -- HACK
                $(P.mkRelDir "external")
              ]
            | otherwise = pure $ P.WalkExclude []

        dirAccum d _ files = zip (map (d P.</>) files)
            <$> mapM
                (\f -> (hmGitRoot <&> (P.</> (d P.</> f)))
                    >>= liftIO . BL.readFile . P.toFilePath
                    >>= fromContents Blob
                    <&> hexStr . objectId)
                files

-- | Returns a map of blob file paths and hashes that are
-- already part of the repository.
-- This is a pair with `latestBlobHashes`.
indexedBlobHashes :: (MonadIO m, MonadCatch m)
    => HMGitT m (ML.Map (P.Path P.Rel P.File) String)
indexedBlobHashes = ifM (hmGitIndexPath >>= P.doesFileExist <&> not)
   (pure ML.empty)
 $ loadIndex
    <&> ML.fromList
        . map (second (hexStr . BL.toStrict . ieSha1) . first iePath . dupe)

-- | Data representing status
data HMGitStatus = HMGitStatus {
    statusChanged :: S.Set (P.Path P.Rel P.File)
  , statusNew     :: S.Set (P.Path P.Rel P.File)
  , statusDeleted :: S.Set (P.Path P.Rel P.File)
  }
  deriving Show

-- | Get status
getStatus :: (MonadIO m, MonadCatch m) => HMGitT m HMGitStatus
getStatus = do
    latest <- latestBlobHashes
    indexed <- indexedBlobHashes
    pure $ HMGitStatus {
        statusChanged = ML.keysSet
            $ ML.filter (not . null)
            $ ML.intersectionWith (\l r -> if l /= r then r else mempty) latest indexed
      , statusNew = ML.keysSet
            $ latest `ML.difference` indexed
      , statusDeleted = ML.keysSet
            $ indexed `ML.difference` latest
      }

