module HMGit.Commands (
    Cmd (..)
) where

import           HMGit.Commands.Plumbing.CatFile.Cmd.Runner    (CatFile (..))
import           HMGit.Commands.Plumbing.HashObject.Cmd.Runner (HashObject (..))
import           HMGit.Commands.Plumbing.LsFiles.Cmd.Runner    (LsFiles (..))
import           HMGit.Internal.Parser                         (ObjectType (..))

data Cmd m = CmdCatFile (CatFile m) String
    | CmdHashObject ObjectType (HashObject m) FilePath
    | CmdLsFiles (LsFiles m) [FilePath]
