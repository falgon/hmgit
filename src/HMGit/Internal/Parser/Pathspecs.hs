module HMGit.Internal.Parser.Pathspecs (
    pathspecs
) where

import           HMGit.Internal.Core                  (HMGitT, hmGitRoot)
import qualified HMGit.Internal.Parser.Pathspecs.Glob as G

import           Control.Exception.Safe               (MonadCatch, catchAny)
import           Control.Monad.Extra                  (anyM)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Functor                         ((<&>))
import qualified Path                                 as P
import qualified Path.IO                              as P

pathspec :: (MonadCatch m, MonadIO m)
    => P.Path P.Abs P.Dir
    -> P.Path P.Abs P.File
    -> String
    -> m Bool
pathspec cDir fpath pat = ($ G.transpile pat) $ maybe (pure False) $ \ir ->
    if not (G.isLiteral ir) then
        (P.stripProperPrefix cDir fpath <&> flip G.match ir . P.toFilePath) `catchAny`
            const (pure False)
    else do
        x <- P.resolveDir cDir pat
        pure $ P.isProperPrefixOf x fpath || P.toFilePath fpath == init (P.toFilePath x)

pathspecs :: (MonadCatch m, MonadIO m)
    => P.SomeBase P.File -- target file path
    -> [String] -- directories or globs
    -> HMGitT m Bool
pathspecs fpath [] = pathspecs fpath ["."]
pathspecs (P.Abs fpath) pat = do
    c <- P.getCurrentDir
    anyM (pathspec c fpath) pat
pathspecs (P.Rel fpath) pat = do
    absPath <- (P.</> fpath) <$> hmGitRoot
    c <- P.getCurrentDir
    anyM (pathspec c absPath) pat
