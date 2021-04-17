{-# LANGUAGE OverloadedStrings #-}
module HMGit.Commands.Porcelain.Init (
    init
) where

import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT)

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy       as BL
import           Prelude                    hiding (init)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))

hmGitInitDir :: FilePath -> (FilePath -> FilePath) -> [FilePath]
hmGitInitDir repoName initDir = map (initDir repoName </>) [
    "objects"
  , "refs" </> "heads"
  ]

init :: FilePath -> HMGitT IO ()
init repoName = do
    initDir <- asks hmGitDir
    liftIO $ mapM_ (createDirectoryIfMissing True) (hmGitInitDir repoName initDir)
        *> BL.writeFile (initDir repoName </> "HEAD") "ref: refs/heads/master"
