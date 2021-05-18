module HMGit.Commands.Porcelain.Add.Core (
    Add (..)
  , addDefault
  , addDryRun
) where

import           HMGit.Internal.Core             (IndexEntry (..),
                                                  ObjectType (..), loadIndex,
                                                  storeIndex, storeObject)
import           HMGit.Internal.Core.Runner      (HMGitT)
import           HMGit.Internal.Parser.Pathspecs (lsMatches)

import           Control.Exception.Safe          (MonadCatch, MonadThrow)
import           Control.Monad                   (forM)
import           Control.Monad.IO.Class          (MonadIO (..))
import qualified Data.ByteString.Lazy            as BL
import           Data.Functor                    ((<&>))
import           Data.List                       (sortBy)
import           Data.Ratio                      (numerator)
import qualified Path                            as P
import qualified Path.IO                         as P
import           System.Posix.Files

newtype Add m = Add { add :: [FilePath] -> HMGitT m () }

existEntries :: (MonadThrow m, MonadIO m)
    => [P.Path P.Rel P.File]
    -> HMGitT m [IndexEntry]
existEntries paths = loadIndex
    <&> filter (not . flip elem paths . iePath)

additionalEntries :: (MonadCatch m, MonadIO m)
    => [P.Path P.Rel P.File]
    -> HMGitT m [IndexEntry]
additionalEntries paths = forM paths $ \p -> do
    sha1 <- liftIO (BL.readFile $ P.toFilePath p)
        >>= storeObject Blob
        <&> BL.fromStrict
    stat <- liftIO $ getFileStatus $ P.toFilePath p
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
        p
    where
        fileModeRegular stat
            | fileMode stat == 0o100664 = 0o100644
            | otherwise = fromIntegral $ fileMode stat

addDefault :: (MonadIO m, MonadCatch m) => Add m
addDefault = Add $ \pats -> do
    paths <- P.getCurrentDir >>= flip lsMatches pats
    ((<>) <$> existEntries paths <*> additionalEntries paths)
        >>= storeIndex . sortBy (\x y -> compare (iePath x) (iePath y))

addDryRun :: (MonadIO m, MonadCatch m) => Add m
addDryRun = Add $ const $ pure ()
