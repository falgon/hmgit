{-# LANGUAGE OverloadedStrings #-}
module HMGit.Commands.Init (
    init
) where

import qualified Data.ByteString.Lazy as BL
import           HMGit.Internals      (hmGitDir)
import           Prelude              hiding (init)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))

hmGitInitDir :: FilePath -> [FilePath]
hmGitInitDir repoName = map (hmGitDir repoName </>) [
    "objects"
  , "refs" </> "heads"
  ]

init :: FilePath -> IO ()
init repoName = mapM_ (createDirectoryIfMissing True) (hmGitInitDir repoName)
    *> BL.writeFile (hmGitDir repoName </> "HEAD") "ref: refs/heads/master"
