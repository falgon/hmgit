{-# LANGUAGE LambdaCase #-}
module HMGit.Commands.Porcelain.Diff.Core (
    Diff (..)
  , showDiff
  , diffShow
) where

import           HMGit.Internal.Core             (HMGitStatus (..), getStatus,
                                                  indexedBlobHashes, loadObject)
import           HMGit.Internal.Core.Runner      (HMGitT)
import           HMGit.Internal.Core.Runner.API  (hmGitRoot)
import           HMGit.Internal.Exceptions       (BugException (..),
                                                  MonadThrowable (..))
import           HMGit.Internal.Parser           (ObjectType (..))
import           HMGit.Internal.Parser.Pathspecs (pathspecs)

import           Control.Exception.Safe          (MonadCatch, catchAny, throw)
import           Control.Monad                   (MonadPlus (..), forM_)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Data.Algorithm.DiffContext      (getContextDiff,
                                                  prettyContextDiff)
import qualified Data.ByteString.Lazy.UTF8       as BLU
import           Data.Functor                    ((<&>))
import qualified Data.Map.Lazy                   as ML
import qualified Data.Set                        as S
import           Data.Void                       (Void)
import qualified Path                            as P
import qualified Path.IO                         as P
import qualified Text.PrettyPrint                as PP

type ShowDiff = FilePath -- file name
    -> String -- first contents
    -> String -- second contents
    -> String

newtype Diff m = Diff { diff :: ShowDiff -> [FilePath] -> HMGitT m () }

showDiff :: String -- source prefix
    -> String -- destination prefix
    -> ShowDiff
showDiff srcP dstP fname lhs rhs = PP.render
    $ prettyContextDiff
        (PP.text $ srcP <> fname)
        (PP.text $ dstP <> fname)
        PP.text
        (getContextDiff 1 (lines lhs) (lines rhs))

diffShow :: (MonadIO m, MonadCatch m, MonadPlus m) => Diff m
diffShow = Diff $ \showDiff' pats -> do
    cDir <- P.getCurrentDir
    root <- hmGitRoot
    changed <- getStatus <&> S.toList . statusChanged
    indexed <- indexedBlobHashes
    forM_ changed $ \p -> whenM ((True <$ pathspecs cDir (P.Rel p) pats) `catchAny` const (pure False)) $
        fromMonad (Nothing :: Maybe Void) (ML.lookup p indexed)
            >>= loadObject
            >>= \case
                (Blob, contents) -> let contents' = BLU.toString contents in do
                    working <- liftIO $ readFile $ P.toFilePath (root P.</> p)
                    liftIO
                        $ putStr
                        $ showDiff' (P.toFilePath p) contents' working
                _ -> lift
                    $ throw
                    $ BugException "The object loaded by diff is expected to be a blob"

