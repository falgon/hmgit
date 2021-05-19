{-# LANGUAGE TemplateHaskell #-}
module HMGit.Internal.Parser.Pathspecs (
    pathspecs
  , lsMatches
) where

import           HMGit.Internal.Core                  (HMGitT)
import           HMGit.Internal.Core.Runner.API       (hmGitDBPath, hmGitRoot)
import           HMGit.Internal.Exceptions            (MonadThrowable (..))
import qualified HMGit.Internal.Parser.Pathspecs.Glob as G
import           HMGit.Internal.Utils                 (foldChoiceM, foldMapM,
                                                       makeRelativeEx, (?*>))

import           Control.Applicative                  (Alternative (..))
import           Control.Exception.Safe               (MonadCatch, catchAny,
                                                       throwString)
import           Control.Monad.Extra                  (filterM, ifM, orM)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Functor                         ((<&>))
import qualified Data.Set                             as S
import           Data.Void                            (Void)
import           Path                                 (Dir, Rel)
import qualified Path                                 as P
import qualified Path.IO                              as P
import           Text.Printf                          (printf)

pathspec :: (MonadCatch m, MonadIO m, Alternative m)
    => P.Path P.Abs P.Dir
    -> P.Path P.Abs P.File
    -> String
    -> m FilePath
pathspec cDir fpath [] = makeRelativeEx (P.toFilePath cDir) (P.toFilePath fpath)
pathspec cDir fpath pat = pathspec' `catchAny`
    const (throwString $ printf "%s does not match in pattern %s" (P.toFilePath fpath) pat)
    where
        pathspec' = do
            x <- P.resolveDir cDir pat -- The path can be resolved regardless of the directory or file
            ir <- G.transpile $ init $ P.toFilePath x
            if G.isLiteral ir then
                (P.isProperPrefixOf x fpath || P.toFilePath fpath == init (P.toFilePath x))
                    ?*> makeRelativeEx (P.toFilePath cDir) (P.toFilePath fpath)
            else
                G.match (P.toFilePath fpath) ir
                    ?*> makeRelativeEx (P.toFilePath cDir) (P.toFilePath fpath)

pathspecs :: (MonadCatch m, MonadIO m, Alternative m)
    => P.Path P.Abs P.Dir -- ^ the specified base directory
    -> P.SomeBase P.File -- ^ the target file path
    -> [String] -- ^ pathspecs
    -> HMGitT m FilePath
pathspecs cDir (P.Abs fpath) [] = pathspec cDir fpath []
pathspecs cDir (P.Rel fpath) [] = hmGitRoot
    <&> (P.</> fpath)
    >>= flip (pathspec cDir) []
pathspecs cDir (P.Abs fpath) pat = foldChoiceM (pathspec cDir fpath) pat
pathspecs cDir (P.Rel fpath) pat = (pathspec
    <$> pure cDir
    <*> ((P.</> fpath) <$> hmGitRoot))
        >>= flip foldChoiceM pat

lsMatch :: (MonadCatch m, MonadIO m)
    => P.Path P.Abs P.Dir -- ^ the specified base directory
    -> String -- ^ pathspec
    -> HMGitT m (S.Set (P.Path P.Abs P.File))
lsMatch _ [] = pure S.empty
lsMatch cDir pat = do
    root <- hmGitRoot
    x <- P.resolveDir cDir pat -- The path can be resolved regardless of the directory or file
    ir <- G.transpile $ init $ P.toFilePath x
    if G.isLiteral ir then let mFile = P.parseAbsFile (init $ P.toFilePath x) in
        ifM (maybe (pure False) P.doesFileExist mFile)
            (fromMonad (Nothing :: Maybe Void) mFile <&> S.singleton) $
            ifM (P.doesDirExist x)
                (P.listDirRecur x >>= excludeDB . snd <&> S.fromList)
                (pure S.empty)
    else
        P.listDirRecur root
            >>= excludeDB . snd
            <&> S.fromList . filter (flip G.match ir . P.toFilePath)
    where
        excludeDB = filterM $ \f -> not <$> orM [
            P.isProperPrefixOf
                <$> hmGitDBPath
                <*> pure f
          , P.isProperPrefixOf
                <$> (hmGitRoot <&> (P.</> $(P.mkRelDir ".stack-work"))) -- HACK
                <*> pure f
          , P.isProperPrefixOf
                <$> (hmGitRoot <&> (P.</> $(P.mkRelDir "test/external"))) -- HACK
                <*> pure f
          ]

lsMatches :: (MonadCatch m, MonadIO m)
    => P.Path P.Abs P.Dir -- ^ the specified base directory
    -> [String] -- ^ pathspecs
    -> HMGitT m (S.Set (P.Path P.Abs P.File))
lsMatches cDir = foldMapM (lsMatch cDir)
