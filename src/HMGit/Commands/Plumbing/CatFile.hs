module HMGit.Commands.Plumbing.CatFile (
    catFile
  , CatFile (..)
  , catFileCmd
) where

import           HMGit.Commands.Plumbing.CatFile.Cmd        (catFileCmd)
import           HMGit.Commands.Plumbing.CatFile.Cmd.Runner (CatFile (..))
import           HMGit.Commands.Plumbing.CatFile.Core       (catFile)
