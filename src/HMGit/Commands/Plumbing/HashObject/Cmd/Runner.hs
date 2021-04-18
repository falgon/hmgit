module HMGit.Commands.Plumbing.HashObject.Cmd.Runner (
    HashObject (..)
) where

import           HMGit.Commands.Plumbing.HashObject.Core (HashObjectOpt (..))

newtype HashObject m = HashObject { getHashObjectRunner :: HashObjectOpt m }
