module HMGit.Internal.Utils (
    stateEmpty
  , foldChoice
) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (MonadPlus (..))
import           Data.Monoid         (Alt (..))

stateEmpty :: (Foldable t, MonadPlus m) => (a, t b) -> m a
stateEmpty x
    | not $ null $ snd x = mzero
    | otherwise = pure $ fst x

foldChoice :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldChoice f = getAlt . foldMap (Alt . f)
