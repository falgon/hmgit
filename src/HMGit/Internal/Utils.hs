module HMGit.Internal.Utils (
    stateEmpty
  , foldChoice
  , hexFormat
  , formatHexStrings
) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (MonadPlus (..))
import           Control.Monad.Extra (concatMapM)
import qualified Data.List.NonEmpty  as LN
import           Data.Monoid         (Alt (..))
import           Data.Word           (Word8)
import           Numeric             (readHex, showHex)
import           Text.Printf         (printf)

stateEmpty :: (Foldable t, MonadPlus m) => (a, t b) -> m a
stateEmpty x
    | not $ null $ snd x = mzero
    | otherwise = pure $ fst x

foldChoice :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldChoice f = getAlt . foldMap (Alt . f)

hexFormat :: Word8 -> String
hexFormat = printf "%02x"

formatBase :: (MonadPlus f, Eq a, Num a, Monoid b) => (a -> c) -> (d -> b -> String) -> d -> Maybe (f c)
formatBase formatter baseShow = fmap (fmap formatter . stateEmpty . LN.head)
    . LN.nonEmpty
    . readHex
    . flip baseShow mempty

formatHexStrings :: (Integral a, Show a) => [a] -> Maybe String
formatHexStrings = fmap concat . concatMapM (formatBase hexFormat showHex)
