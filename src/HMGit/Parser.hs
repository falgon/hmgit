{-# LANGUAGE TupleSections #-}
module HMGit.Parser (
    ObjectType (..)
  , objectParser
) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8  as BLU
import           Data.Char                  (chr, ord)
import           Data.List                  (isPrefixOf)
import           Data.Void
import           Data.Word                  (Word8)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Error      as M

data ObjectType = Blob
    | Commit
    | Tree
    deriving (Eq, Enum)

instance Show ObjectType where
    show Blob   = "blob"
    show Commit = "commit"
    show Tree   = "tree"

instance Read ObjectType where
    readsPrec _ s
        | "blob" `isPrefixOf` s = [(Blob, drop 4 s)]
        | "commit" `isPrefixOf` s = [(Commit, drop 6 s)]
        | "tree" `isPrefixOf` s = [(Tree, drop 4 s)]
        | otherwise = []

pNull :: M.Parsec Void BL.ByteString Word8
pNull = M.single 0

pSpace :: M.Parsec Void BL.ByteString Word8
pSpace = M.single $ fromIntegral $ ord ' '

pDecimals :: (Read i, Integral i) => M.Parsec Void BL.ByteString i
pDecimals = read . map (chr . fromIntegral) <$> M.many (M.choice $ map (M.single . fromIntegral . ord) ['1'..'9'])

pObjectTypes :: M.Parsec Void BL.ByteString ObjectType
pObjectTypes = read . BLC.unpack <$> M.choice (map (MC.string . BLU.fromString . show) [ Blob .. Tree ])

pHeader :: (Read i, Integral i) => M.Parsec Void BL.ByteString (ObjectType, i)
pHeader = (,) <$> pObjectTypes <*> (pSpace *> pDecimals <* pNull)

objectParser :: M.Parsec Void BL.ByteString (ObjectType, BL.ByteString)
objectParser = do
    (objType, size) <- pHeader
    (objType,) . BL.pack <$> M.count size M.anySingle <* M.eof

