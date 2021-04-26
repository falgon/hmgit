module HMGit.Commands.Plumbing.LsFiles.Cmd.Runner (
    LsFiles (..)
) where

import           HMGit.Commands.Plumbing.LsFiles.Core (LsFilesOpt (..))

newtype LsFiles m = LsFiles { getLsFilesRunner :: LsFilesOpt m }
