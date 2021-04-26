module HMGit.Commands.Plumbing (
    PlumbingArgs (..)
  , Plumbing (..)
) where

import           HMGit.Internal.Core    (HMGitT)
import           HMGit.Internal.Parser  (ObjectType)

import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as BL
import           System.FilePath.Glob   (Pattern)

data PlumbingArgs = PAObject ObjectType BL.ByteString
    | PAFilePatterns [Pattern]

class Plumbing a where
    runPlumbing :: (MonadIO m, MonadThrow m)
        => a m
        -> PlumbingArgs
        -> HMGitT m ()
