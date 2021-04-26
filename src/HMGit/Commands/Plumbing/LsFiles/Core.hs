{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module HMGit.Commands.Plumbing.LsFiles.Core (
    LsFilesOpt (..)
  , lsFilesShow
  , lsFilesDetail
  , lsFiles
) where

import           HMGit.Commands.Plumbing    (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core        (HMGitT,
                                             getCurrentDirectoryFromHMGit,
                                             loadIndex)
import           HMGit.Internal.Parser      (IndexEntry (..))
import           HMGit.Internal.Utils       (formatHexByteString)

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans        (lift)
import           Data.Bits                  (shiftL, (.&.))
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List                  (isPrefixOf)
import           Data.List.Extra            (dropPrefix)
import           System.FilePath            ((</>))
import           System.FilePath.Glob       (CompOptions (..),
                                             MatchOptions (..), Pattern,
                                             compPosix, compileWith, matchPosix,
                                             matchWith)
import           Text.Printf                (printf)

newtype LsFilesOpt m = LsFilesOpt ([Pattern] -> HMGitT m ())

instance Plumbing LsFilesOpt where
    runPlumbing (LsFilesOpt f) (PAFilePatterns patterns) = f patterns
    runPlumbing _ _                                      = pure ()

match :: Pattern -> FilePath -> Bool
match = matchWith $ matchPosix { matchDotsImplicitly = True }

compile :: String -> Pattern
compile = compileWith $ compPosix { recursiveWildcards = True }

isMatchAnyOf :: FilePath -> [Pattern] -> Bool
isMatchAnyOf _ []       = True
isMatchAnyOf s patterns = any (`match` s) patterns

putLs :: MonadIO m => IndexEntry -> FilePath -> m ()
putLs idx currentDir = liftIO
    $ putStrLn
    $ dropPrefix currentDir
    $ BLC.unpack
    $ iePath idx

putLsDetail :: (MonadThrow m, MonadIO m) => IndexEntry -> FilePath -> m ()
putLsDetail e currentDir = case formatHexByteString $ ieSha1 e of
    Left err -> throw err
    Right s -> liftIO
        $ putStrLn
        $ printf "%6o %s %d\t%s" (ieMode e) s ((ieFlags e `shiftL` 12) .&. 3)
        $ dropPrefix currentDir
        $ BLC.unpack
        $ iePath e

lsFilesBase :: forall m. (MonadThrow m, MonadIO m) => (IndexEntry -> FilePath -> m ()) -> LsFilesOpt m
lsFilesBase printer = LsFilesOpt $ \patterns -> do
    f <- execLs patterns <$> getCurrentDirectoryFromHMGit
    loadIndex >>= lift . mapM_ f
    where
        execLs patterns currentDir e = let path = BLC.unpack $ iePath e in
            when (path `isMatchAnyOf` patterns && currentDir `isPrefixOf` path)
              $ printer e currentDir

lsFilesShow :: (MonadThrow m, MonadIO m) => LsFilesOpt m
lsFilesShow = lsFilesBase putLs

lsFilesDetail :: (MonadThrow m, MonadIO m) => LsFilesOpt m
lsFilesDetail = lsFilesBase putLsDetail

lsFiles :: (MonadThrow m, MonadIO m) => LsFilesOpt m -> [FilePath] -> HMGitT m ()
lsFiles lsFilesOpt = runPlumbing lsFilesOpt
    . PAFilePatterns
    . map (\x -> compile $ if "*" `isPrefixOf` x then "**" </> x else x)
