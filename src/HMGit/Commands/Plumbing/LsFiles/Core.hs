module HMGit.Commands.Plumbing.LsFiles.Core (

) where

import           HMGit.Commands.Plumbing (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core     (HMGitT, liftIOUnit, loadIndex)
import           HMGit.Internal.Parser   (IndexEntry (..))

import           Control.Exception.Safe  (MonadThrow, throw)
import           Control.Monad           (forM_)
import           Data.Bits               (shiftL, (.&.))
import           Text.Printf             (printf)

newtype LsFilesOpt m = LsFilesOpt (HMGitT IO (m ()))

instance Plumbing LsFilesOpt where
    runPlumbing (LsFilesOpt f) PAUnit = f
    runPlumbing _ _                   = liftIOUnit $ pure ()

lsFiles :: MonadThrow m => LsFilesOpt m
lsFiles = LsFilesOpt $ loadIndex >>= either throw execLs
    where
        execLs entries = liftIOUnit $ forM_ entries $ \e ->
            let stage = (ieFlags e `shiftL` 12) .&. 3 in pure ()
            -- putStrLn $ printf "%6o {} {}\t{}" $ (ieMode e) (ieSha1 e) stage (iePath e)

-- lsFilesDetail :: MonadThrow m => LsFilesOpt m
--lsFilesDetail = LsFilesOpt
