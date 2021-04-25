module HMGit.Commands.Plumbing (
    PlumbingArgs (..)
  , Plumbing (..)
) where

import           HMGit.Internal.Core    (HMGitT)
import           HMGit.Internal.Parser  (ObjectType)

import           Control.Exception.Safe (MonadThrow)
import qualified Data.ByteString.Lazy   as BL

data PlumbingArgs = PAObject ObjectType BL.ByteString
    | PAUnit

class Plumbing a where
    runPlumbing :: MonadThrow m
        => a m
        -> PlumbingArgs
        -> HMGitT IO (m ())
