module HMGit.Internal.Parser.Pathspecs (
    pathspecs
  , removeDots
) where

import qualified HMGit.Internal.Parser.Pathspecs.Glob as G

import           Control.Applicative                  (Alternative (..))
import           Control.Exception.Safe               (MonadCatch, MonadThrow,
                                                       catchAny, throwString)
import           Control.Monad.Extra                  (ifM)
import           Control.Monad.Fix                    (fix)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.State.Lazy             (State, StateT, evalState,
                                                       evalStateT, gets, modify)
import           Control.Monad.Trans                  (lift)
import           Data.Foldable                        (Foldable (..), asum)
import           Data.Functor                         ((<&>))
import           Data.List                            (intercalate)
import qualified Data.Sequence                        as S
import           Data.Tuple.Extra                     (dupe, first, second)
import qualified Path                                 as P
import qualified Path.IO                              as P
import           System.Directory                     (makeRelativeToCurrentDirectory)
import           System.FilePath                      (isAbsolute)

someRelFile :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.File
    -> m (P.Path P.Rel P.File)
someRelFile (P.Rel p) = pure p
someRelFile (P.Abs p) = liftIO (makeRelativeToCurrentDirectory $ P.toFilePath p)
    >>= P.parseRelFile

someRelDir :: (MonadIO m, MonadThrow m)
    => P.SomeBase P.Dir
    -> m (P.Path P.Rel P.Dir)
someRelDir (P.Rel p) = pure p
someRelDir (P.Abs p) = liftIO (makeRelativeToCurrentDirectory $ P.toFilePath p) -- strip?
    >>= P.parseRelDir

-- ルートだとあわない -> abs path はだめ
removeDots' :: MonadThrow m
    => StateT String (State (S.Seq String)) (m String)
removeDots' = ifM (gets isAbsolute) (pure $ throwString "invalid argument: abs path") $
    fix $ \f -> ifM (gets null) stackToRes $ do
        d <- gets (takeWhile (/='/')) <* putDropped
        if d == ".." then
            ifM (lift $ gets S.null)
                -- ここで abs path に変換して abs path かえす? SomeBase とか
                (pure $ throwString "cannot get parent dir")
                (popDir *> f)
        else
            lift (modify (S.|> d)) *> f
    where
        putDropped = modify $ \s -> case dropWhile (/='/') s of
            '/':s' -> s'
            ss     -> ss

        popDir = lift $ modify $ \s -> case S.viewr s of
            s' S.:> _ -> s'
            S.EmptyR  -> S.empty

        stackToRes = lift -- 結果が空だったときはカレント
            $ gets
            $ \x -> let y = intercalate "/" $ toList x in
                pure $ if null y then "." else y

type RelationalFilePath = FilePath

removeDots :: MonadThrow m
    => RelationalFilePath
    -> m String
removeDots = flip evalState S.empty . evalStateT removeDots'

-- ../ が未対応
pathspecs :: (MonadCatch m, MonadIO m, Alternative m)
    => P.Path P.Rel P.Dir
    -> P.Path P.Rel P.File
    -> String
    -> m Bool
pathspecs _ _ [] = pure True
pathspecs cDir s pat = ($ G.transpile pat) $ maybe (pure False) $ \ir -> do
    s' <- P.stripProperPrefix cDir s `catchAny` const (pure s)
    if not (G.isLiteral ir) then pure $ G.match (P.toFilePath s') ir else do
        asum [ -- ireake?
            removeDots pat
                >>= P.parseSomeDir
                >>= someRelDir
                <&> (uncurry (||)
                    . first (flip P.isProperPrefixOf s')
                    . second ((== P.toFilePath s') . init . P.toFilePath) . dupe)
          , P.parseSomeFile pat -- asum deha hiroenai node catch?
                >>= someRelFile
                <&> (==s')
          ]

