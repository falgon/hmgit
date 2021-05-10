module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFiles (..)
  , lsFilesShow
  , lsFilesDetail
  , lsFiles
) where

import           HMGit.Internal.Core             (HMGitT,
                                                  getCurrentDirFromHMGit,
                                                  loadIndex)
import           HMGit.Internal.Parser           (IndexEntry (..))
import           HMGit.Internal.Parser.Pathspecs (pathspecs)
import           HMGit.Internal.Utils            (formatHexByteString)

import           Control.Applicative             (Alternative)
import           Control.Exception.Safe          (MonadCatch, MonadThrow)
import           Control.Monad.Extra             (anyM, whenM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Data.Bits                       (shiftL, (.&.))
import qualified Path                            as P
import           Text.Printf                     (printf)

newtype LsFiles m = LsFiles ([String] -> HMGitT m ())

putLs :: (MonadThrow m, MonadIO m)
    => IndexEntry
    -> P.Path P.Rel P.Dir
    -> m ()
putLs idx currentDir = P.stripProperPrefix currentDir (iePath idx)
    >>= liftIO . putStrLn . P.toFilePath

putLsDetail :: (MonadThrow m, MonadIO m)
    => IndexEntry
    -> P.Path P.Rel P.Dir
    -> m ()
putLsDetail e currentDir = do
    s <- formatHexByteString $ ieSha1 e
    P.stripProperPrefix currentDir (iePath e)
        >>= liftIO
         . putStrLn
         . printf "%6o %s %d\t%s" (ieMode e) s ((ieFlags e `shiftL` 12) .&. 3)
         . P.toFilePath

lsFilesBase :: (MonadCatch m, MonadIO m, Alternative m)
    => (IndexEntry -> P.Path P.Rel P.Dir -> m ())
    -> LsFiles m
lsFilesBase printer = LsFiles $ \pat -> do
    currentDir <- getCurrentDirFromHMGit
    loadIndex >>= mapM_ (execLs pat currentDir)
    where
        execLs pat currentDir e = let path = iePath e in
            flip whenM (lift $ printer e currentDir) $
                (&& currentDir `P.isProperPrefixOf` path)
                    <$> if null pat then pure True else anyM (pathspecs currentDir path) pat

lsFilesShow :: (MonadCatch m, MonadIO m, Alternative m) => LsFiles m
lsFilesShow = lsFilesBase putLs

lsFilesDetail :: (MonadCatch m, MonadIO m, Alternative m) => LsFiles m
lsFilesDetail = lsFilesBase putLsDetail

lsFiles :: LsFiles m
    -> [FilePath]
    -> HMGitT m ()
lsFiles (LsFiles f) = f

