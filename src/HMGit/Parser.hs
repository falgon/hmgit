{-# LANGUAGE TupleSections #-}
module HMGit.Parser (
    ObjectType (..)
  , ObjectParser
  , objectParser
  , treeParser
) where

import qualified Codec.Binary.UTF8.String   as S
import           Control.Monad              (MonadPlus (..))
import           Control.Monad.Extra        (concatMapM, ifM)
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
import           Numeric                    (readHex, readOct, showHex)
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import           Text.Printf                (printf)

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

type ObjectParser = M.Parsec Void BL.ByteString

pNull :: ObjectParser Word8
pNull = M.single 0

pSpace :: ObjectParser Word8
pSpace = M.single $ fromIntegral $ ord ' '

pDecimals :: (Read i, Integral i) => ObjectParser i
pDecimals = read . map (chr . fromIntegral) <$> M.many (M.choice $ map (M.single . fromIntegral . ord) ['0'..'9'])

pObjectTypes :: ObjectParser ObjectType
pObjectTypes = read . BLC.unpack <$> M.choice (map (MC.string . BLU.fromString . show) [ Blob .. Tree ])

pHeader :: (Read i, Integral i) => ObjectParser (ObjectType, i)
pHeader = (,) <$> pObjectTypes <*> (pSpace *> pDecimals <* pNull)

objectParser :: ObjectParser (ObjectType, BL.ByteString)
objectParser = do
    (objType, size) <- pHeader
    (objType,) . BL.pack <$> M.count size M.anySingle <* M.eof

stateEmpty :: (Foldable t, MonadPlus m) => (a, t b) -> m a
stateEmpty x
    | not $ null $ snd x = mzero
    | otherwise = pure $ fst x

treeParser :: Int -> ObjectParser [(CMode, FilePath, String)]
treeParser limit = runMaybeT treeParser'
    >>= maybe (fail "failed to parse octet value of cmode") pure
    where
        treeParser' = flip unfoldrM 0 $ \limitCount ->
            ifM ((||) <$> pure (limitCount >= limit) <*> lift M.atEnd) (pure Nothing) $ do
                cmode <- stateEmpty =<< LN.head <$> MaybeT (LN.nonEmpty . readOct . show <$> pDecimals) <* lift pSpace
                (.) Just . (.) (, succ limitCount) . (cmode,,)
                    <$> lift (S.decode <$> M.manyTill M.anySingle pNull)
                    <*> hexDigest

        hexDigest = let formatter = printf "%02x" :: Word8 -> String in
            MaybeT (mapM (LN.nonEmpty . readHex . flip showHex mempty) <$> M.count 20 M.anySingle)
                >>= concatMapM (fmap formatter . stateEmpty . LN.head)
