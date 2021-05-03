{-# LANGUAGE OverloadedStrings #-}
module HMGit.Commands.Porcelain.Init.Core (
    RepositoryName
  , Init (..)
  , initDefault
  , initQuiet
  , init
) where

import           HMGit.Internal.Core.Runner (HMGitT)

import           Control.Exception.Safe     (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO (..))
import qualified Data.ByteString.Lazy       as BL
import           Prelude                    hiding (init)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))

type RepositoryName = String

newtype Init m = Init (RepositoryName -> HMGitT m ())

initBase :: MonadIO m
    => String
    -> RepositoryName
    -> HMGitT m ()
initBase gitName repoName = let hmGitRoot = repoName </> gitName in liftIO
    $ mapM_ (createDirectoryIfMissing True) (dirs hmGitRoot)
    *> BL.writeFile (hmGitRoot </> "HEAD") "ref: refs/heads/master"
    where
        dirs hmGitRoot = map (hmGitRoot </>) [
            "objects"
          , "refs" </> "heads"
          ]

initDefault :: (MonadThrow m, MonadIO m) => String -> Init m
initDefault gitName = Init $ \repoName -> initBase gitName repoName
    *> liftIO (putStrLn ("initialized empty repository: " <> repoName))

initQuiet :: MonadIO m => String -> Init m
initQuiet gitName = Init $ initBase gitName

init :: (String -> Init m)
    -> String
    -> RepositoryName
    -> HMGitT m ()
init initOpt gitName dir = case initOpt gitName of Init f -> f dir
