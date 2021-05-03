module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFiles (..)
  , lsFilesShow
  , lsFilesDetail
  , lsFiles
) where

import           HMGit.Commands.Plumbing (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core     (HMGitT, getCurrentDirectoryFromHMGit,
                                          loadIndex)
import           HMGit.Internal.Parser   (IndexEntry (..))
import           HMGit.Internal.Utils    (formatHexByteString)

import           Control.Exception.Safe  (MonadThrow)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Trans     (lift)
import           Data.Bits               (shiftL, (.&.))
import           Data.List               (isPrefixOf)
import qualified Path                    as P
import           System.FilePath         ((</>))
import           System.FilePath.Glob    (CompOptions (..), MatchOptions (..),
                                          Pattern, compPosix, compileWith,
                                          matchPosix, matchWith)
import           Text.Printf             (printf)

newtype LsFiles m = LsFiles ([Pattern] -> HMGitT m ())

instance Plumbing LsFiles where
    runPlumbing (LsFiles f) (PAFilePatterns patterns) = f patterns
    runPlumbing _ _                                   = pure ()

match :: Pattern -> P.Path t b -> Bool
match p = matchWith (matchPosix { matchDotsImplicitly = True }) p
    . P.toFilePath

compile :: String -> Pattern
compile = compileWith $ compPosix { recursiveWildcards = True }

isMatchAnyOf :: P.Path t b -> [Pattern] -> Bool
isMatchAnyOf _ []       = True
isMatchAnyOf s patterns = any (`match` s) patterns

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

lsFilesBase :: (MonadThrow m, MonadIO m)
    => (IndexEntry -> P.Path P.Rel P.Dir -> m ())
    -> LsFiles m
lsFilesBase printer = LsFiles $ \patterns -> do
    currentDir <- getCurrentDirectoryFromHMGit
    loadIndex >>= mapM_ (execLs patterns currentDir)

    where
        execLs patterns currentDir e = let path = iePath e in
            when (path `isMatchAnyOf` patterns && currentDir `P.isProperPrefixOf` path)
              $ lift $ printer e currentDir

lsFilesShow :: (MonadThrow m, MonadIO m) => LsFiles m
lsFilesShow = lsFilesBase putLs

lsFilesDetail :: (MonadThrow m, MonadIO m) => LsFiles m
lsFilesDetail = lsFilesBase putLsDetail

lsFiles :: (MonadThrow m, MonadIO m) => LsFiles m -> [FilePath] -> HMGitT m ()
lsFiles lsFilesOpt = runPlumbing lsFilesOpt
    . PAFilePatterns
    . map (\x -> compile $ if "*" `isPrefixOf` x then "**" </> x else x)
