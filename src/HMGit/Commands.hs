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

-- | Commands supported by HMGit
data Cmd m = CmdInit (String -> Init m) RepositoryName -- ^ init
    | CmdAdd (Add m) AddCfg -- ^ add
    | CmdCatFile (CatFile m) String -- ^ cat-file
    | CmdHashObject ObjectType (HashObject m) FilePath -- ^ hash-object
    | CmdLsFiles (LsFiles m) LsFilesCfg -- ^ ls-files
    | CmdStatus (Status m) StatusCfg -- ^ status
    | CmdDiff (Diff m) DiffCfg -- ^ diff
    | CmdCommit (Commit m) CommitCfg -- ^ commit
