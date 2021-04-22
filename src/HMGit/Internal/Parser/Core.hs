module HMGit.Internal.Parser.Core (
    ParseException (..)
  , ByteStringParser
  , pNull
  , pSpace
  , pDecimals
) where

import           HMGit.Internal.Utils   (foldChoice)

import           Control.Exception.Safe (Exception)
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (chr, ord)
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
pDecimals = read . map (chr . fromIntegral) <$> M.many (foldChoice (M.single . fromIntegral . ord) ['0'..'9'])
