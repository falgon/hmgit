module HMGit.Commands.Plumbing.CatFile.Cmd.Runner (
    CatFile (..)
) where

import           Data.String                          (IsString (..))
import           HMGit.Commands.Plumbing.CatFile.Core (CatOpt (..),
                                                       catOptObject)

newtype CatFile m = CatFile { getCatFileRunner :: CatOpt m }

instance Applicative m => IsString (CatFile m) where
    fromString = CatFile . catOptObject . read
