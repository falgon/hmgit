module HMGit.Internal.Parser.Pathspecs (
    pathspecs
  , lsMatches
) where

import           HMGit.Internal.Core                  (HMGitT, hmGitRoot)
import           HMGit.Internal.Exceptions            (MonadThrowable (..))
import qualified HMGit.Internal.Parser.Pathspecs.Glob as G
import           HMGit.Internal.Utils                 (foldChoiceM,
                                                       makeRelativeEx, (?*>))

import           Control.Applicative                  (Alternative (..))
import           Control.Exception.Safe               (MonadCatch, catchAny,
                                                       throwString)
import           Control.Monad.Extra                  (concatMapM, ifM)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Functor                         ((<&>))
import           Data.Void                            (Void)
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
    => P.Path P.Abs P.Dir -- the specified base directory
    -> P.SomeBase P.File -- target file path
    -> [String] -- pathspecs
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
    => P.Path P.Abs P.Dir -- the specified base directory
    -> String -- pathspec
    -> HMGitT m [P.Path P.Rel P.File]
lsMatch _ [] = pure []
lsMatch cDir pat = do
    root <- hmGitRoot
    x <- P.resolveDir cDir pat -- The path can be resolved regardless of the directory or file
    ir <- G.transpile $ init $ P.toFilePath x
    if G.isLiteral ir then let mFile = P.parseAbsFile (init $ P.toFilePath x) in
        ifM (maybe (pure False) P.doesFileExist mFile)
            (fromMonad (Nothing :: Maybe Void) mFile >>= P.stripProperPrefix root <&> (:[])) $
            ifM (P.doesDirExist x)
                (P.listDirRecur x >>= mapM (P.stripProperPrefix root) . snd)
                (pure [])
    else
        P.listDirRecur root
            >>= mapM (P.stripProperPrefix root)
                . filter (flip G.match ir . P.toFilePath) . snd
        -- excludeDB = filterM (fmap not . ((P.isProperPrefixOf <$> hmGitDBPath) <*>) . pure)

lsMatches :: (MonadCatch m, MonadIO m)
    => P.Path P.Abs P.Dir -- the specified base directory
    -> [String] -- pathspecs
    -> HMGitT m [P.Path P.Rel P.File]
lsMatches cDir = concatMapM (lsMatch cDir)
