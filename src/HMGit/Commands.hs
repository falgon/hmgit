module HMGit.Commands (
    Cmd (..)
) where

import           HMGit.Commands.Plumbing.CatFile.Core    (CatFile)
import           HMGit.Commands.Plumbing.HashObject.Core (HashObject)
import           HMGit.Commands.Plumbing.LsFiles.Core    (LsFiles, LsFilesCfg)
import           HMGit.Commands.Porcelain.Add.Core       (Add, AddCfg)
import           HMGit.Commands.Porcelain.Commit.Core    (Commit, CommitCfg)
import           HMGit.Commands.Porcelain.Diff.Core      (Diff, DiffCfg)
import           HMGit.Commands.Porcelain.Init.Core      (Init, RepositoryName)
import           HMGit.Commands.Porcelain.Status.Core    (Status, StatusCfg)
import           HMGit.Internal.Parser                   (ObjectType (..))

data Cmd m = CmdInit (String -> Init m) RepositoryName
    | CmdAdd (Add m) AddCfg
    | CmdCatFile (CatFile m) String
    | CmdHashObject ObjectType (HashObject m) FilePath
    | CmdLsFiles (LsFiles m) LsFilesCfg
    | CmdStatus (Status m) StatusCfg
    | CmdDiff (Diff m) DiffCfg
    | CmdCommit (Commit m) CommitCfg
