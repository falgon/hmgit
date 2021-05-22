{-# LANGUAGE OverloadedStrings, TupleSections #-}
module HMGit.Internal.Parser.Object (
    ObjectType (..)
  , objectParser
  , treeParser
) where

import           HMGit.Internal.Parser.Core
import           HMGit.Internal.Utils       (foldChoice, hexStr)

import           Control.Monad.Extra        (ifM)
import           Control.Monad.Loops        (unfoldrM)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8  as BLU
import           Data.List                  (isPrefixOf)
import           Data.Tuple.Extra           (secondM)
import qualified Path                       as P
import           Prelude                    hiding (null)
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC

-- | The Git object type.
-- Currently, it does not support tags.
data ObjectType = Blob -- ^ Blob object
    | Commit -- ^ Commit object
    | Tree -- ^ Tree object
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

objectTypes :: ByteStringParser ObjectType
objectTypes = read . BLC.unpack
    <$> foldChoice (MC.string . BLU.fromString . show) [ Blob .. Tree ]

header :: Num i => ByteStringParser (ObjectType, i)
header = (,)
    <$> objectTypes
    <*> (space *> decimal <* null)

-- | Object binary parser
objectParser :: ByteStringParser (ObjectType, BL.ByteString)
objectParser = (header >>= secondM (fmap BL.pack . flip M.count M.anySingle))
    <* M.eof

-- | Tree binary parser
treeParser :: Int -> ByteStringParser [(CMode, P.Path P.Rel P.File, String)]
treeParser limit = flip unfoldrM 0 $ \lim ->
    ifM ((lim >= limit ||) <$> M.atEnd) (pure Nothing) $ do
        cmode <- octal <* space
        prel <- relFile
        Just . (, succ lim) . (cmode, prel,) . hexStr <$> M.count 20 M.anySingle

