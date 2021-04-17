module HMGit.Commands.Plumbing.CatFile.Cmd.Runner (
    CatFile (..)
) where

import           Data.String                          (IsString (..))
import           HMGit.Commands.Plumbing.CatFile.Core (CatOpt (..),
                                                       catOptObject)

newtype CatFile = CatFile { getCatFileRunner :: CatOpt }

instance IsString CatFile where
    fromString = CatFile . catOptObject . read
