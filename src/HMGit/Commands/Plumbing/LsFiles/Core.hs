module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFilesOpt (..)
  , lsFilesShow
  , lsFilesDetail
  , lsFiles
) where

import           HMGit.Commands.Plumbing    (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core        (HMGitT, liftException, liftIOUnit,
                                             loadIndex)
import           HMGit.Internal.Parser      (IndexEntry (..))
import           HMGit.Internal.Utils       (formatHexByteString)

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad              (forM_)
import           Data.Bits                  (shiftL, (.&.))
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Text.Printf                (printf)

newtype LsFilesOpt m = LsFilesOpt (HMGitT IO (m ()))

instance Plumbing LsFilesOpt where
    runPlumbing (LsFilesOpt f) PAUnit = f
    runPlumbing _ _                   = liftIOUnit $ pure ()

lsFilesDetail :: MonadThrow m => LsFilesOpt m
lsFilesDetail = LsFilesOpt $ loadIndex >>= either (liftException . throw) execLs
    where
        execLs entries = liftIOUnit $ forM_ entries $ \e -> case formatHexByteString $ ieSha1 e of
            Left err -> throw err
            Right s -> let stage = (ieFlags e `shiftL` 12) .&. 3 in do
                putStrLn $ printf "%6o %s %d\t%s" (ieMode e) s stage (BLC.unpack $ iePath e)

lsFilesShow :: MonadThrow m => LsFilesOpt m
lsFilesShow = LsFilesOpt $ loadIndex
    >>= either (liftException . throw) (liftIOUnit . mapM_ (BLC.putStrLn . iePath))

lsFiles :: MonadThrow m => LsFilesOpt m -> HMGitT IO (m ())
lsFiles lsFilesOpt = runPlumbing lsFilesOpt PAUnit
