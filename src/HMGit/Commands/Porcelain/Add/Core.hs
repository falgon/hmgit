module HMGit.Commands.Porcelain.Add.Core (
    Add (..)
  , AddCfg (..)
  , addDefault
  , addDryRun
) where

import           HMGit.Internal.Core             (IndexEntry (..),
                                                  ObjectInfo (..),
                                                  ObjectType (..), fromContents,
                                                  loadIndex, storeIndex,
                                                  storeObject)
import           HMGit.Internal.Core.Runner      (HMGitT, hmGitRoot)
import           HMGit.Internal.Parser.Pathspecs (lsMatches)

import           Control.Exception.Safe          (MonadCatch, MonadThrow)
import           Control.Monad                   (forM)
import           Control.Monad.IO.Class          (MonadIO (..))
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Functor                    ((<&>))
import           Data.List                       (sortBy)
import           Data.Ratio                      (numerator)
import qualified Data.Set                        as S
import qualified Path                            as P
import qualified Path.IO                         as P
import           System.Posix.Files
import           Text.Printf                     (printf)

newtype Add m = Add { add :: AddCfg -> HMGitT m () }

newtype AddCfg = AddCfg {
    addPathspecs :: [FilePath]
  }

type BlobGenerator m = BL.ByteString -> HMGitT m B.ByteString

existEntries :: (MonadThrow m, MonadIO m)
    => S.Set (P.Path P.Abs P.File)
    -> HMGitT m [IndexEntry]
existEntries paths = do
    root <- hmGitRoot
    paths' <- mapM (P.stripProperPrefix root) $ S.toList paths
    loadIndex <&> filter (not . flip elem paths' . iePath)

additionalEntries :: (MonadCatch m, MonadIO m)
    => BlobGenerator m
    -> S.Set (P.Path P.Abs P.File)
    -> HMGitT m [IndexEntry]
additionalEntries blobGen paths = forM (S.toList paths) $ \p -> do
    sha1 <- liftIO (BL.readFile $ P.toFilePath p)
        >>= blobGen
        <&> BL.fromStrict
    stat <- liftIO $ getFileStatus $ P.toFilePath p
    root <- hmGitRoot
    p' <- P.stripProperPrefix root p
    pure $ IndexEntry
        (fromIntegral $ fromEnum $ toRational $ statusChangeTime stat)
        (fromIntegral $ numerator $ toRational $ statusChangeTimeHiRes stat)
        (fromIntegral $ fromEnum $ toRational $ modificationTime stat)
        (fromIntegral $ numerator $ toRational $ modificationTimeHiRes stat)
        (fromIntegral $ deviceID stat)
        (fromIntegral $ fileID stat)
        (fileModeRegular stat)
        (fromIntegral $ fileOwner stat)
        (fromIntegral $ fileGroup stat)
        (fromIntegral $ fileSize stat)
        sha1
        (fromIntegral $ length $ P.toFilePath p)
        p'
    where
        fileModeRegular stat
            | fileMode stat == 0o100664 = 0o100644
            | otherwise = fromIntegral $ fileMode stat

addDefault :: (MonadIO m, MonadCatch m) => Add m
addDefault = Add $ \addCfg -> do
    paths <- P.getCurrentDir >>= flip lsMatches (addPathspecs addCfg)
    ((<>) <$> existEntries paths <*> additionalEntries (storeObject Blob) paths)
        >>= storeIndex . sortBy (\x y -> compare (iePath x) (iePath y))

addDryRun :: (MonadCatch m, MonadIO m) => Add m
addDryRun = Add $ \addCfg -> P.getCurrentDir
    >>= flip lsMatches (addPathspecs addCfg)
    >>= additionalEntries (fmap objectId . fromContents Blob)
    >>= mapM_ (liftIO . putStrLn . printf "add '%s'" . P.toFilePath . iePath)
