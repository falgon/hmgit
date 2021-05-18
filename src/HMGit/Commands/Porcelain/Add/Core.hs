module HMGit.Commands.Porcelain.Add.Core (
    Add (..)
  , addDefault
) where

import           HMGit.Internal.Core             (loadIndex)
import           HMGit.Internal.Core.Runner      (HMGitT)
import           HMGit.Internal.Parser.Pathspecs (pathspecs)

import qualified Path                            as P

newtype Add m = Add { add :: [FilePath] -> HMGitT m () }

addDefault = undefined

{-
addDefault :: Add m
addDefault = Add $ \ps -> do
    cDir <- P.getCurrentDir
    entries <- loadIndex
  -}
