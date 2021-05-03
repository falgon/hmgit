module HMGit.Commands (
    Cmd (..)
) where

import           HMGit.Commands.Plumbing.CatFile.Core    (CatFile)
import           HMGit.Commands.Plumbing.HashObject.Core (HashObject)
import           HMGit.Commands.Plumbing.LsFiles.Core    (LsFiles)
import           HMGit.Internal.Parser                   (ObjectType (..))

data Cmd m = CmdCatFile (CatFile m) String
    | CmdHashObject ObjectType (HashObject m) FilePath
    | CmdLsFiles (LsFiles m) [FilePath]
