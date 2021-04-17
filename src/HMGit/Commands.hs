module HMGit.Commands (
    Cmd (..)
) where

import           HMGit.Commands.Plumbing.CatFile.Cmd.Runner (CatFile)

data Cmd = CmdCatFile CatFile String
