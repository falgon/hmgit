module HMGit.Commands.Plumbing (
    Plumbing (..)
) where

import           HMGit.Internal.Core    (HMGitT)
import           HMGit.Internal.Parser  (ObjectType)

import           Control.Exception.Safe (MonadThrow)
import qualified Data.ByteString.Lazy   as BL

class Plumbing a where
    runPlumbing :: MonadThrow m
        => a m
        -> ObjectType
        -> BL.ByteString
        -> HMGitT IO (m ())
