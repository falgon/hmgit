{-# LANGUAGE TupleSections #-}
module HMGit.Internal.Parser (
    ObjectType (..)
  , ObjectParser
  , ObjectParseException
  , objectParser
  , treeParser
) where

import           HMGit.Internal.Utils       (foldChoice, formatHexStrings,
                                             stateEmpty)

import qualified Codec.Binary.UTF8.String   as S
import           Control.Exception.Safe     (Exception)
import           Control.Monad.Extra        (ifM)
import           Control.Monad.Loops        (unfoldrM)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8  as BLU
import           Data.Char                  (chr, ord)
import           Data.List                  (isPrefixOf)
import qualified Data.List.NonEmpty         as LN
import           Data.Tuple.Extra           (secondM)
import           Data.Word                  (Word8)
import           Numeric                    (readOct)
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC

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

newtype ObjectParseException = TreeParser String
    deriving (Eq, Ord, Show)

instance M.ShowErrorComponent ObjectParseException where
    showErrorComponent (TreeParser s) = "tree object parse error: " <> s

instance Exception ObjectParseException

type ObjectParser = M.Parsec ObjectParseException BL.ByteString

pNull :: ObjectParser Word8
pNull = M.single 0

pSpace :: ObjectParser Word8
pSpace = M.single $ fromIntegral $ ord ' '

pDecimals :: (Read i, Integral i) => ObjectParser i
pDecimals = read . map (chr . fromIntegral) <$> M.many (foldChoice (M.single . fromIntegral . ord) ['0'..'9'])

pObjectTypes :: ObjectParser ObjectType
pObjectTypes = read . BLC.unpack <$> foldChoice (MC.string . BLU.fromString . show) [ Blob .. Tree ]

pHeader :: (Read i, Integral i) => ObjectParser (ObjectType, i)
pHeader = (,) <$> pObjectTypes <*> (pSpace *> pDecimals <* pNull)

objectParser :: ObjectParser (ObjectType, BL.ByteString)
objectParser = (secondM (fmap BL.pack . flip M.count M.anySingle) =<< pHeader) <* M.eof

treeParser :: Int -> ObjectParser [(CMode, FilePath, String)]
treeParser limit = runMaybeT treeParser'
    >>= maybe (M.customFailure $ TreeParser "failed to parse octet value of cmode") pure
    where
        treeParser' = flip unfoldrM 0 $ \limitCount ->
            ifM ((limitCount >= limit ||) <$> lift M.atEnd) (pure Nothing) $ do
                cmode <- stateEmpty =<< LN.head <$> MaybeT (LN.nonEmpty . readOct . show <$> pDecimals') <* lift pSpace
                (.) Just . (.) (, succ limitCount) . (cmode,,)
                    <$> lift (S.decode <$> M.manyTill M.anySingle pNull)
                    <*> MaybeT (formatHexStrings <$> M.count 20 M.anySingle)

        pDecimals' = pDecimals :: ObjectParser Integer
