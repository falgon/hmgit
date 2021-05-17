module HMGit.Internal.Parser.Pathspecs (
    pathspecs
) where

import           HMGit.Internal.Core                  (HMGitT, hmGitRoot)
import qualified HMGit.Internal.Parser.Pathspecs.Glob as G
import           HMGit.Internal.Utils                 (foldChoiceM,
                                                       makeRelativeEx, (?*>))

import           Control.Applicative                  (Alternative (..))
import           Control.Exception.Safe               (MonadCatch, catchAny,
                                                       throwString)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Functor                         ((<&>))
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
            x <- P.resolveDir cDir pat
            ir <- G.transpile $ init $ P.toFilePath x
            if G.isLiteral ir then
                (P.isProperPrefixOf x fpath || P.toFilePath fpath == init (P.toFilePath x))
                    ?*> makeRelativeEx (P.toFilePath cDir) (P.toFilePath fpath)
            else
                G.match (P.toFilePath fpath) ir
                    ?*> makeRelativeEx (P.toFilePath cDir) (P.toFilePath fpath)

pathspecs :: (MonadCatch m, MonadIO m, Alternative m)
    => P.Path P.Abs P.Dir -- the current directory
    -> P.SomeBase P.File -- target file path
    -> [String] -- directories or globs
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

