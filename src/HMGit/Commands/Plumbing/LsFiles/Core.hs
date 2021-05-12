module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFiles (..)
  , lsFilesShow
  , lsFilesDetail
) where

import           HMGit.Internal.Core             (HMGitT, loadIndex)
import           HMGit.Internal.Parser           (IndexEntry (..))
import           HMGit.Internal.Parser.Pathspecs (pathspecs)
import           HMGit.Internal.Utils            (formatHexByteString)

import           Control.Applicative             (Alternative (..))
import           Control.Exception.Safe          (MonadCatch, catchAny)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Bits                       (shiftL, (.&.))
import qualified Path                            as P
import qualified Path.IO                         as P
import           Text.Printf                     (printf)

newtype LsFiles m = LsFiles { lsFiles :: [FilePath] -> HMGitT m () }

putLs :: (MonadIO m)
    => IndexEntry
    -> FilePath
    -> HMGitT m ()
putLs _ fp = liftIO $ putStrLn fp

putLsDetail :: (MonadCatch m, MonadIO m)
    => IndexEntry
    -> FilePath
    -> HMGitT m ()
putLsDetail idx fp = do
    s <- formatHexByteString $ ieSha1 idx
    liftIO
        $ putStrLn
        $ printf "%6o %s %d\t%s" (ieMode idx) s ((ieFlags idx `shiftL` 12) .&. 3) fp

lsFilesBase :: (MonadCatch m, MonadIO m, Alternative m)
    => (IndexEntry -> FilePath -> HMGitT m ())
    -> LsFiles m
lsFilesBase printer = LsFiles $ \pat -> do
    cDir <- P.getCurrentDir
    loadIndex
        >>= mapM_ (\e -> (pathspecs cDir (P.Rel (iePath e)) pat >>= printer e) `catchAny`
                const (pure ()))

lsFilesShow :: (MonadCatch m, MonadIO m, Alternative m) => LsFiles m
lsFilesShow = lsFilesBase putLs

lsFilesDetail :: (MonadCatch m, MonadIO m, Alternative m) => LsFiles m
lsFilesDetail = lsFilesBase putLsDetail

