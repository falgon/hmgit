module HMGit.Commands (
    Cmd (..)
) where

import           HMGit.Commands.Plumbing.CatFile.Core    (CatFile)
import           HMGit.Commands.Plumbing.HashObject.Core (HashObject)
import           HMGit.Commands.Plumbing.LsFiles.Core    (LsFiles)
import           HMGit.Commands.Porcelain.Add.Core       (Add)
import           HMGit.Commands.Porcelain.Diff.Core      (Diff)
import           HMGit.Commands.Porcelain.Init.Core      (Init, RepositoryName)
import           HMGit.Commands.Porcelain.Status.Core    (Status)
import           HMGit.Internal.Parser                   (ObjectType (..))

data Cmd m = CmdInit (String -> Init m) RepositoryName
    | CmdAdd (Add m) [FilePath]
    | CmdCatFile (CatFile m) String
    | CmdHashObject ObjectType (HashObject m) FilePath
    | CmdLsFiles (LsFiles m) [FilePath]
    | CmdStatus (Status m) [FilePath]
    | CmdDiff (Diff m) [FilePath]
