{-# LANGUAGE ExplicitNamespaces, FlexibleContexts, Rank2Types, TypeOperators #-}
module HMGit.Internal.Parser.Core.ByteString (
    ParseException (..)
  , runByteStringParser
  , ByteStringParser
  , null
  , space
  , decimal
  , octal
  , fromBinaryGetter
  , relFile
) where

import           HMGit.Internal.Exceptions (MonadThrowable (..))
import           HMGit.Internal.Utils      (first3M)

import qualified Codec.Binary.UTF8.String  as S
import           Control.Exception.Safe    (Exception, MonadThrow)
import           Control.Monad             (void, (>=>))
import           Control.Natural           (type (~>))
import qualified Data.Binary.Get           as BG
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (chr, ord)
import           Data.Char                 (digitToInt)
import           Data.List                 (foldl')
import           Data.Tuple.Extra          (thd3)
import           Data.Typeable             (Typeable)
import           Data.Void                 (Void)
import           Data.Word                 (Word8)
import qualified Path                      as P
import           Prelude                   hiding (null)
import qualified Text.Megaparsec           as M
import qualified Text.Megaparsec.Byte      as M

data ParseException = TreeParser String
    | IndexParser String
    | MasterHashParser String
    deriving (Eq, Ord, Show)

instance M.ShowErrorComponent ParseException where
    showErrorComponent (TreeParser s)       = "tree object parse error: " <> s
    showErrorComponent (IndexParser s)      = "index parse error: " <> s
    showErrorComponent (MasterHashParser s) = "master hash parse error: " <> s

instance Exception ParseException

type ByteStringParser = M.Parsec ParseException BL.ByteString

runByteStringParser :: (
    MonadThrow m
  , Show e
  , M.ShowErrorComponent e
  , Typeable e
  , M.VisualStream s
  , M.TraversableStream s
  , Typeable s
  , Show s
  , Show (M.Token s)
  )
    => M.Parsec e s a
    -> P.Path b t
    -> s
    -> m a
runByteStringParser p fp = fromMonad (Nothing :: Maybe Void)
    . M.runParser p (P.toFilePath fp)

-- | null
null :: ByteStringParser Word8
null = M.single 0

-- | M.space is fine but we just want to match 0x20
space :: ByteStringParser ()
space = void $ M.single $ fromIntegral $ ord ' '

-- | decimal
decimal :: Num i => ByteStringParser i
decimal = foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt (chr (fromIntegral c)))) 0
    <$> M.some M.digitChar

-- | octal
octal :: Num i => ByteStringParser i
octal = foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt (chr (fromIntegral c)))) 0
    <$> M.some M.octDigitChar

-- | Natural transformation from Get to parsec
fromBinaryGetter :: (String -> ParseException) -> BG.Get ~> ByteStringParser
fromBinaryGetter pException binGetter = M.getInput
    >>= either (M.customFailure . pException . thd3) (first3M M.setInput >=> pure . thd3)
    . BG.runGetOrFail binGetter

-- | parse relative file path until null
relFile :: ByteStringParser (P.Path P.Rel P.File)
relFile = M.manyTill M.anySingle null
    >>= maybe (M.customFailure $ TreeParser "failed to parse relative file path") pure
     . P.parseRelFile . S.decode
