module HMGit.Commands.Plumbing.HashObject (
    hashObject
  , HashObject (..)
  , hashObjectCmd
) where

import           HMGit.Commands.Plumbing.HashObject.Cmd  (hashObjectCmd)
import           HMGit.Commands.Plumbing.HashObject.Core (HashObject (..),
                                                          hashObject)
