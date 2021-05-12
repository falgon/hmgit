module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFiles (..)
  , lsFilesShow
  , lsFilesDetail
  , lsFiles
) where

import           HMGit.Internal.Core             (HMGitT, loadIndex,
                                                  showIndexPath)
import           HMGit.Internal.Parser           (IndexEntry (..))
import           HMGit.Internal.Parser.Pathspecs (pathspecs)
import           HMGit.Internal.Utils            (formatHexByteString)

import           Control.Exception.Safe          (MonadCatch)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Bits                       (shiftL, (.&.))
import qualified Path                            as P
import           Text.Printf                     (printf)

newtype LsFiles m = LsFiles ([String] -> HMGitT m ())

putLs :: (MonadCatch m, MonadIO m)
    => IndexEntry
    -> HMGitT m ()
putLs idx = showIndexPath idx
    >>= liftIO
     . putStrLn
     . P.toFilePath

putLsDetail :: (MonadCatch m, MonadIO m)
    => IndexEntry
    -> HMGitT m ()
putLsDetail idx = do
    s <- formatHexByteString $ ieSha1 idx
    showIndexPath idx
        >>= liftIO
         . putStrLn
         . printf "%6o %s %d\t%s" (ieMode idx) s ((ieFlags idx `shiftL` 12) .&. 3)
         . P.toFilePath

lsFilesBase :: (MonadCatch m, MonadIO m)
    => (IndexEntry -> HMGitT m ())
    -> LsFiles m
lsFilesBase printer = LsFiles $ \pat -> loadIndex
    >>= mapM_ (\e -> whenM (pathspecs (P.Rel (iePath e)) pat) $ printer e)

lsFilesShow :: (MonadCatch m, MonadIO m) => LsFiles m
lsFilesShow = lsFilesBase putLs

lsFilesDetail :: (MonadCatch m, MonadIO m) => LsFiles m
lsFilesDetail = lsFilesBase putLsDetail

lsFiles :: LsFiles m
    -> [FilePath]
    -> HMGitT m ()
lsFiles (LsFiles f) = f

