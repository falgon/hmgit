module HMGit.Internal.Parser.Core (
    ParseException (..)
  , ByteStringParser
  , pNull
  , pSpace
  , pDecimals
  , fromBinaryGetter
) where

import           HMGit.Internal.Utils   (first3M, foldChoice)

import           Control.Exception.Safe (Exception)
import           Control.Monad          ((>=>))
import qualified Data.Binary.Get        as BG
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (chr, ord)
import           Data.Tuple.Extra       (dupe, first, second, snd3, thd3)
import           Data.Word              (Word8)
import qualified Text.Megaparsec        as M

data ParseException = TreeParser String
    | IndexParser String
    deriving (Eq, Ord, Show)

instance M.ShowErrorComponent ParseException where
    showErrorComponent (TreeParser s)  = "tree object parse error: " <> s
    showErrorComponent (IndexParser s) = "index parse error: " <> s

instance Exception ParseException

type ByteStringParser = M.Parsec ParseException BL.ByteString

pNull :: ByteStringParser Word8
pNull = M.single 0

pSpace :: ByteStringParser Word8
pSpace = M.single $ fromIntegral $ ord ' '

pDecimals :: (Read i, Integral i) => ByteStringParser i
pDecimals = read . map (chr . fromIntegral)
    <$> M.many (foldChoice (M.single . fromIntegral . ord) ['0'..'9'])

fromBinaryGetter :: (String -> ParseException) -> BG.Get a -> ByteStringParser (BG.ByteOffset, a)
fromBinaryGetter pException binGetter = M.getInput
    >>= either (M.customFailure . pException . thd3) (first3M M.setInput >=> pure . first snd3 . second thd3 . dupe)
    . BG.runGetOrFail binGetter
