module HMGit.Commands.Plumbing (
    Plumbing (..)
) where

import           HMGit.Internal.Core    (HMGitT)
import           HMGit.Internal.Parser  (ObjectType)

import           Control.Exception.Safe (MonadThrow)
import qualified Data.ByteString.Lazy   as BL
import           Data.String            (IsString (..))

class Plumbing a where
    runPlumbing :: MonadThrow m => a -> ObjectType -> BL.ByteString -> HMGitT IO (m ())
