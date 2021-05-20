module HMGit.Commands.Porcelain.Commit (
    Commit (..)
  , CommitCfg (..)
  , commitCmd
) where

import           HMGit.Commands.Porcelain.Commit.Cmd  (commitCmd)
import           HMGit.Commands.Porcelain.Commit.Core (Commit (..),
                                                       CommitCfg (..))
