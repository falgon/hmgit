{-# LANGUAGE ExplicitForAll, TupleSections #-}
module HMGit.Internal.Utils (
    stateEmpty
  , foldChoice
  , formatHexStrings
  , formatHexByteString
  , formatHexByteString'
  , first3M
) where

import           HMGit.Internal.Exceptions  (MonadThrowable (..))

import           Control.Applicative        (Alternative (..))
import           Control.Exception.Safe     (MonadThrow, StringException (..))
import           Control.Monad              (MonadPlus (..))
import           Control.Monad.Extra        (concatMapM)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Char                  (ord)
import qualified Data.List.NonEmpty         as LN
import           Data.Monoid                (Alt (..))
import           Data.Word                  (Word8)
import           GHC.Stack                  (callStack)
import           Numeric                    (readHex, showHex)
import           Text.Printf                (printf)

stateEmpty :: (Foldable t, MonadPlus m) => (a, t b) -> m a
stateEmpty x
    | not $ null $ snd x = mzero
    | otherwise = pure $ fst x

foldChoice :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
foldChoice f = getAlt . foldMap (Alt . f)

formatBase :: (MonadPlus f, Eq a, Num a, Monoid b) => (a -> c) -> (d -> b -> String) -> d -> Maybe (f c)
formatBase formatter baseShow = fmap (fmap formatter . stateEmpty . LN.head)
    . LN.nonEmpty
    . readHex
    . flip baseShow mempty

formatHexStrings :: MonadThrow m => (Integral a, Show a) => [a] -> m String
formatHexStrings = fmap concat
    . fromMonad (Just $ StringException "cannot parse a hex value" callStack)
    . concatMapM (formatBase (printf "%02x" :: Word8 -> String) showHex)

formatHexByteString :: MonadThrow m => BL.ByteString -> m String
formatHexByteString = formatHexStrings . map ord . BLC.unpack

formatHexByteString' :: MonadThrow m => B.ByteString -> m String
formatHexByteString' = formatHexStrings . map ord . BC.unpack

first3M :: Functor m => (a -> m a') -> (a, b, c) -> m (a', b, c)
first3M f (x,y,z) = (,y,z) <$> f x
