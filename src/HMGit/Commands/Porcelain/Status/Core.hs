{-# LANGUAGE LambdaCase, TupleSections #-}
module HMGit.Commands.Porcelain.Status.Core (
    Status (..)
  , StatusCfg (..)
  , statusDefault
  , statusShort
) where

import           HMGit.Internal.Core             (HMGitStatus (..), getStatus)
import           HMGit.Internal.Core.Runner      (HMGitT)
import           HMGit.Internal.Parser.Pathspecs (pathspecs)

import           Control.Applicative             (Alternative (..))
import           Control.Exception.Safe          (MonadCatch, catchAny)
import           Control.Monad                   (foldM, zipWithM_, (>=>))
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Control.Monad.Trans.Reader      (ReaderT (..), ask)
import           Data.Functor                    ((<&>))
import qualified Data.List.NonEmpty              as LN
import qualified Data.Set                        as S
import qualified Path                            as P
import qualified Path.IO                         as P
import           Text.Printf                     (printf)

newtype Status m = Status { status :: StatusCfg -> HMGitT m () }

newtype StatusCfg = StatusCfg {
    statusPathspecs :: [String]
  }

type StatusSelector = HMGitStatus -> S.Set (P.Path P.Rel P.File)

type StatusShow m = ReaderT (HMGitStatus, [String], P.Path P.Abs P.Dir, String -> IO ()) (HMGitT m)

statusShow :: (MonadIO m, MonadCatch m, Alternative m)
    => StatusSelector
    -> String
    -> StatusShow m ()
statusShow sctor title = do
    (hs, pats, cDir, printer) <- ask
    foldM (\acc p -> lift (pathspecs cDir (P.Rel p) pats <&> (:acc)) `catchAny` const (pure acc))
        mempty
        (S.toList (sctor hs))
        <&> LN.nonEmpty
        >>= \case
            Nothing -> pure ()
            Just fs -> liftIO (putStr title)
                *> mapM_ (liftIO . printer) fs

statusDefault :: (MonadIO m, MonadCatch m, Alternative m) => Status m
statusDefault = Status (cfg . statusPathspecs >=> runReaderT statusShow')
    where
        cfg pats = (, pats, , liftIO . putStrLn . printf "\t%s")
            <$> getStatus
            <*> P.getCurrentDir
        statusShow' = zipWithM_ statusShow
            [ statusChanged, statusNew, statusDeleted ]
            [ "Changes files:\n", "New files:\n", "Deleted files:\n" ]

statusShort :: (MonadCatch m, MonadIO m, Alternative m) => Status m
statusShort = Status $ \statusCfg -> do
    cDir <- P.getCurrentDir
    st <- getStatus
    zipWithM_ (\x -> runReaderT (statusShow x ""))
        [ statusChanged
        , statusNew
        , statusDeleted
        ]
        [ (st, statusPathspecs statusCfg, cDir, putStrLn . printf " M %s")
        , (st, statusPathspecs statusCfg, cDir, putStrLn . printf "?? %s")
        , (st, statusPathspecs statusCfg, cDir, putStrLn . printf " D %s")
        ]
