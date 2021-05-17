{-# LANGUAGE ExplicitNamespaces, OverloadedStrings, Rank2Types,
             TypeOperators #-}
module HMGit.Internal.Parser.Index (
    IndexEntry (..)
  , indexParser
  , putIndex
) where

import           HMGit.Internal.Parser.Core.ByteString
import           HMGit.Internal.Utils                  (foldMapM)

import qualified Codec.Binary.UTF8.String              as BUS
import           Control.Monad.Extra                   (ifM, orM, replicateM_)
import           Control.Monad.Loops                   (unfoldM)
import           Control.Natural                       (type (~>))
import           Crypto.Hash.SHA1                      (hashlazy)
import qualified Data.Binary.Get                       as BG
import qualified Data.Binary.Put                       as BP
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (ord)
import           Data.Tuple.Extra                      (thd3)
import           Data.Word                             (Word16, Word32)
import qualified Path                                  as P
import qualified Text.Megaparsec                       as M
import           Text.Printf                           (printf)

-- ^ Index format, ref. https://github.com/git/git/blob/v2.17.1/Documentation/technical/index-format.txt#L9-L17
data IndexHeader = IndexHeader {
    ihSignature  :: BL.ByteString   -- ^ The signature is { 'D', 'I', 'R', 'C' } (stands for "dircache")
  , ihVersion    :: Word32          -- ^ The current supported versions are 2, 3 and 4.
  , ihNumEntries :: Word32          -- ^ Number of index entries.
  }
  deriving Show

putIndexHeader :: Word32 -> BP.Put
putIndexHeader len = BP.putByteString "DIRC"
    *> BP.putWord32be 2
    *> BP.putWord32be len

-- ^ Index entry, ref. https://github.com/git/git/blob/v2.17.1/Documentation/technical/index-format.txt#L38
data IndexEntry = IndexEntry {
    ieCtimeS :: Word32          -- ^ the last time a file's metadata changed,  this is stat(2) data
  , ieCtimeN :: Word32          -- ^ nanosecond fractions, this is stat(2) data
  , ieMTimeS :: Word32          -- ^ mtime seconds, the last time a file's data changed, this is stat(2) data
  , ieMTimeN :: Word32          -- ^ mtime nanosecond fractions, this is stat(2) data
  , ieDev    :: Word32          -- ^ this is stat(2) data
  , ieIno    :: Word32          -- ^ this is stat(2) data
  , ieMode   :: Word32          -- ^ mode, split into (high to low bits)
  , ieUid    :: Word32          -- ^ this is stat(2) data
  , ieGid    :: Word32          -- ^ this is stat(2) data
  , ieSize   :: Word32          -- ^ This is the on-disk size from stat(2), truncated to 32-bit.
  , ieSha1   :: BL.ByteString   -- ^ 160-bit SHA-1 for the represented object
  , ieFlags  :: Word16          -- ^ A 16-bit 'flags' field split into (high to low bits)
  , iePath   :: P.Path P.Rel P.File
  }
  deriving Show

putIndexEntry :: IndexEntry -> BP.Put
putIndexEntry ie = BP.putWord32be (ieCtimeS ie)
    *> BP.putWord32be (ieCtimeN ie)
    *> BP.putWord32be (ieMTimeS ie)
    *> BP.putWord32be (ieMTimeN ie)
    *> BP.putWord32be (ieDev ie)
    *> BP.putWord32be (ieIno ie)
    *> BP.putWord32be (ieMode ie)
    *> BP.putWord32be (ieUid ie)
    *> BP.putWord32be (ieGid ie)
    *> BP.putWord32be (ieSize ie)
    *> BP.putLazyByteString (ieSha1 ie)
    *> BP.putWord16be (ieFlags ie)
    *> BP.putByteString (BC.pack $ P.toFilePath $ iePath ie)
    *> replicateM_ (packedLen - 62 - pLen) (BP.putWord8 0)
    where
        pLen = length $ P.toFilePath $ iePath ie
        packedLen = ((62 + pLen + 8) `div` 8) * 8

putIndex :: Foldable t => t IndexEntry -> BP.Put
putIndex ies = putIndexHeader (fromIntegral $ length ies)
    *> foldMapM putIndexEntry ies

fromBinaryGetter' :: BG.Get ~> ByteStringParser
fromBinaryGetter' = fromBinaryGetter IndexParser

indexHeader :: ByteStringParser IndexHeader
indexHeader = setBody
    >> M.count 12 M.anySingle
    >>= either (M.customFailure . IndexParser . thd3) idxHeader
     .  BG.runGetOrFail (IndexHeader <$> BG.getLazyByteString 4 <*> BG.getWord32be <*> BG.getWord32be)
     .  BL.pack
    where
        setBody = do
            indexData <- M.getInput
            let (body, sha1) = BL.splitAt (BL.length indexData - 20) indexData
            if hashlazy body /= BL.toStrict sha1 then
                M.customFailure $ IndexParser "invalid index checksum"
            else
                M.setInput body

        idxHeader :: (BL.ByteString, BG.ByteOffset, IndexHeader) -> ByteStringParser IndexHeader
        idxHeader (unconsumed, nConsumed, val)
            | BL.null unconsumed && nConsumed == 12 = pure val
            | otherwise = M.customFailure $ IndexParser "expected consumed size number is 12"

lookSignature :: IndexHeader -> ByteStringParser IndexHeader
lookSignature ih
    | ihSignature ih == "DIRC" = pure ih
    | otherwise = M.customFailure $ IndexParser "invalid index signature"

lookVersion :: IndexHeader -> ByteStringParser IndexHeader
lookVersion ih
    | ihVersion ih == 2 = pure ih
    | otherwise = M.customFailure $ IndexParser "unknown index version"

indexBody :: Word32 -> ByteStringParser [IndexEntry]
indexBody expectedEntriesNum = unfoldM (ifM stopConditions (pure Nothing) idxField)
    >>= lookNumEntries
    where
        stopConditions = orM [
            M.atEnd
          , (62>) . BL.length <$> M.getInput
            -- > If the first byte is 'A'..'Z' the extension is optional and can be ignored.
            -- ref. https://github.com/git/git/blob/v2.17.1/Documentation/technical/index-format.txt#L28-L29
          , M.option False (True <$ M.lookAhead (M.satisfy ((`elem` map ord ['A'..'Z']) . fromIntegral)))
          ]

        idxField = do
            entry <- fromBinaryGetter' $ IndexEntry
                <$> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getWord32be
                <*> BG.getLazyByteString 20
                <*> BG.getWord16be
            fmap entry . P.parseRelFile . BUS.decode
                <$> M.manyTill (M.anySingleBut 0) pNull
                <*  M.count' 0 7 pNull

        lookNumEntries :: [IndexEntry] -> ByteStringParser [IndexEntry]
        lookNumEntries entries
            | length entries /= fromIntegral expectedEntriesNum = M.customFailure
                $ IndexParser
                $ printf "expected number of entries is %d, but got %d entries" expectedEntriesNum
                $ length entries
            | otherwise = pure entries

indexParser :: ByteStringParser [IndexEntry]
indexParser = indexHeader
    >>= lookSignature
    >>= lookVersion
    >>= indexBody . ihNumEntries
