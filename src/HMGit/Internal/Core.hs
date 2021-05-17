{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, TemplateHaskell,
             TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module HMGit.Internal.Core (
    HMGitT
  , hmGitRoot
  , getCurrentDirFromHMGit
  , runHMGit
  , ObjectInfo (..)
  , fromContents
  , storeObject
  , loadObject
  , loadTree
  , loadIndex
  , storeIndex
  , HMGitStatus (..)
  , indexedBlobHashes
  , getStatus
) where

import           HMGit.Internal.Core.Runner
import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser      (IndexEntry (..), ObjectType (..),
                                             indexParser, objectParser,
                                             putIndex, runByteStringParser,
                                             treeParser)
import           HMGit.Internal.Utils       (formatHexByteString', strictOne)

import           Codec.Compression.Zlib     (compress, decompress)
import           Control.Exception.Safe     (MonadCatch, MonadThrow, catch,
                                             catchAny, throw)
import           Control.Monad              (MonadPlus)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans        (lift)
import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.Binary.Put            as BP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU
import qualified Data.ByteString.UTF8       as BU
import           Data.Functor               (($>), (<&>))
import           Data.List                  (isPrefixOf)
import qualified Data.Map.Lazy              as ML
import qualified Data.Set                   as S
import           Data.Tuple.Extra           (both, dupe, first, firstM, second,
                                             secondM)
import           Path                       (Dir, File, Rel)
import qualified Path                       as P
import qualified Path.IO                    as P
import           Prelude                    hiding (init)
import           System.Posix.Types         (CMode (..))
import           Text.Printf                (printf)

hmGitObjectsDirLength :: Int
hmGitObjectsDirLength = 2

data ObjectInfo = ObjectInfo {
    objectId   :: BU.ByteString
  , objectData :: BL.ByteString
  , objectPath :: P.Path P.Abs P.File
  }

objectFormat :: ObjectType
    -> BL.ByteString
    -> BL.ByteString
objectFormat objType contents = mconcat [
    BLU.fromString $ show objType
  , " "
  , BLU.fromString $ show $ BL.length contents
  , BL.singleton 0
  , contents
  ]

hashToObjectPath :: MonadCatch m
    => B.ByteString
    -> HMGitT m (Either (P.Path P.Abs P.Dir) (P.Path P.Abs P.File))
hashToObjectPath sha1
    | B.length sha1 < hmGitObjectsDirLength = lift
        $ throw
        $ invalidArgument
        $ printf "hash prefix must be %d or more characters"
            hmGitObjectsDirLength
    | otherwise = do
        (dir, fname) <- firstM P.parseRelDir
            $ both BU.toString
            $ B.splitAt hmGitObjectsDirLength sha1
        ((\x y -> Right $ x P.</> $(P.mkRelDir "objects") P.</> dir P.</> y)
            <$> hmGitDBPath
            <*> lift (P.parseRelFile fname))
            `catch` \e@(P.InvalidRelFile fp) -> if null fp
                then hmGitDBPath <&> Left . (P.</> ($(P.mkRelDir "objects") P.</> dir))
                else lift $ throw e

fromContents :: MonadCatch m
    => ObjectType
    -> BL.ByteString
    -> HMGitT m ObjectInfo
fromContents objType contents = do
    objId <- BU.fromString <$> formatHexByteString' (hashlazy objFormat)
    hashToObjectPath objId
        >>= either
            (const $ throw $ BugException "fromContents: hashToObjectPath must give the Abs file")
            (pure . ObjectInfo objId (compress objFormat))
    where
        objFormat = objectFormat objType contents

storeObject :: (MonadIO m, MonadCatch m)
    => ObjectType
    -> BL.ByteString
    -> HMGitT m B.ByteString
storeObject objType contents = do
    objInfo <- fromContents objType contents
    P.createDirIfMissing True (P.parent $ objectPath objInfo)
        *> liftIO (BL.writeFile (P.toFilePath $ objectPath objInfo) $ objectData objInfo)
        $> objectId objInfo

loadObject :: (MonadIO m, MonadCatch m, MonadPlus m)
    => B.ByteString
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
            $ printf "objects %s not found or multiple object (%d) with prefix %s"
                (BU.toString sha1) (B.length sha1) (BU.toString sha1)

        findTargetObject dir fname = P.listDirRel dir
            >>= strictOne . filter (isPrefixOf fname . P.toFilePath) . snd
            <&> (dir P.</>)

loadTree :: MonadThrow m
    => BL.ByteString
    -> HMGitT m [(CMode, P.Path P.Rel P.File, String)]
loadTree body = hmGitTreeLim
    >>= flip (`runByteStringParser` $(P.mkRelFile "index")) body
     . treeParser

loadIndex :: (MonadIO m, MonadThrow m) => HMGitT m [IndexEntry]
loadIndex = do
    fname <- hmGitIndexPath
    liftIO (BL.readFile $ P.toFilePath fname)
        >>= runByteStringParser indexParser fname

storeIndex :: (MonadIO m, Foldable t)
    => t IndexEntry
    -> HMGitT m ()
storeIndex es = hmGitIndexPath
    >>= liftIO . flip B.writeFile (idxData <> digest) . P.toFilePath
    where
        idxData = BL.toStrict $ BP.runPut $ putIndex es
        digest = hashlazy $ BL.fromStrict idxData

data HMGitStatus = HMGitStatus {
    statusChanged :: S.Set (P.Path P.Rel P.File)
  , statusNew     :: S.Set (P.Path P.Rel P.File)
  , statusDeleted :: S.Set (P.Path P.Rel P.File)
  }
  deriving Show

-- Currently `latestBlobHashes` does not support gitignore and submodule,
-- so we are embedding content to ignore directly in the code.
-- Comments HACK below are the relevant part.
latestBlobHashes :: (MonadIO m, MonadCatch m)
    => HMGitT m (ML.Map (P.Path P.Rel P.File) B.ByteString)
latestBlobHashes = hmGitRoot
    >>= P.walkDirAccumRel (Just dirPred) accum
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

        accum d _ files = zip (map (d P.</>) files)
            <$> mapM
                (\f -> (hmGitRoot <&> (P.</> (d P.</> f)))
                    >>= liftIO . BL.readFile . P.toFilePath
                    >>= fmap objectId . fromContents Blob)
                files

indexedBlobHashes :: (MonadIO m, MonadCatch m)
    => HMGitT m (ML.Map (P.Path P.Rel P.File) B.ByteString)
indexedBlobHashes = loadIndex
    >>= mapM
        (secondM (fmap BU.fromString . formatHexByteString' . BL.toStrict . ieSha1)
        . first iePath
        . dupe
        )
    <&> ML.fromList

getStatus :: (MonadIO m, MonadCatch m) => HMGitT m HMGitStatus
getStatus = do
    latest <- latestBlobHashes
    indexed <- indexedBlobHashes
    pure $ HMGitStatus {
        statusChanged = ML.keysSet
            $ ML.filter (not . B.null)
            $ ML.intersectionWith (\l r -> if l /= r then r else mempty) latest indexed
      , statusNew = ML.keysSet
            $ latest `ML.difference` indexed
      , statusDeleted = ML.keysSet
            $ indexed `ML.difference` latest
      }

