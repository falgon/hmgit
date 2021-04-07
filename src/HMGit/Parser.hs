{-# LANGUAGE TupleSections #-}
module HMGit.Parser (
    ObjectType (..)
  , objectParser
  , treeParser
) where

import qualified Codec.Binary.UTF8.String   as S
import           Control.Monad              (MonadPlus (..))
import           Control.Monad.Loops        (unfoldrM)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8  as BLU
import           Data.Char                  (chr, ord)
import           Data.List                  (isPrefixOf)
import qualified Data.List.NonEmpty         as LN
import           Data.Void
import           Data.Word                  (Word8)
import           Numeric                    (readOct)
import           System.Posix.Types         (CMode (..))
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

treeParser :: Int -> M.Parsec Void BL.ByteString [(CMode, FilePath, BL.ByteString)]
treeParser limit = runMaybeT treeParser'
    >>= maybe (fail "failed to parse octet value of cmode") pure
    where
        treeParser' = flip unfoldrM 0 $ \limitCount -> do
            cmode <- LN.head <$> MaybeT (LN.nonEmpty . readOct . show <$> pDecimals)
            if not $ null $ snd cmode then mzero else M.choice
                [ Nothing <$ lift M.eof
                , if limitCount >= limit then pure Nothing else lift mzero
                , (.) Just . (.) (, succ limitCount) . (fst cmode,,)
                    <$> lift (pSpace *> (S.decode <$> M.manyTill M.anySingle pNull))
                    <*> lift (BL.pack <$> M.count 20 M.anySingle)
                ]
