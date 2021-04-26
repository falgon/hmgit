module HMGit.Commands.Plumbing.CatFile.Cmd.Runner (
    CatFile (..)
) where

import           Control.Monad.IO.Class               (MonadIO)
import           Data.String                          (IsString (..))
import           HMGit.Commands.Plumbing.CatFile.Core (CatOpt (..),
                                                       catOptObject)

newtype CatFile m = CatFile { getCatFileRunner :: CatOpt m }

instance MonadIO m => IsString (CatFile m) where
    fromString = CatFile . catOptObject . read
