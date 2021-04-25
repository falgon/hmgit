{-# LANGUAGE OverloadedStrings, TupleSections #-}
module HMGit.Internal.Parser.Index (
    IndexEntry (..)
  , indexParser
) where

import           HMGit.Internal.Parser.Core

import           Control.Monad.Extra        (ifM)
import           Control.Monad.Loops        (unfoldM)
import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.Binary.Get            as BG
import qualified Data.ByteString.Lazy       as BL
import           Data.Functor               ((<&>))
import           Data.Word                  (Word16, Word32)
import qualified Text.Megaparsec            as M
import           Text.Printf                (printf)

data IndexHeader = IndexHeader {
    ihSignature  :: BL.ByteString
  , ihVersion    :: Word32
  , ihNumEntries :: Word32
  }

data IndexEntry = IndexEntry {
    ieCtimeS :: Word32
  , ieCtimeN :: Word32
  , ieMTimeS :: Word32
  , ieMTimeN :: Word32
  , ieDev    :: Word32
  , ieIno    :: Word32
  , ieMode   :: Word32
  , ieUid    :: Word32
  , ieGid    :: Word32
  , ieSize   :: Word32
  , ieSha1   :: BL.ByteString
  , ieFlags  :: Word16
  , iePath   :: BL.ByteString
  }

fromBinaryGetter' :: BG.Get a -> ByteStringParser (BG.ByteOffset, a)
fromBinaryGetter' = fromBinaryGetter IndexParser

indexHeader :: ByteStringParser IndexHeader
indexHeader = setBody *> M.count 12 M.anySingle
    <&> BG.runGet (IndexHeader <$> BG.getLazyByteString 4 <*> BG.getWord32be <*> BG.getWord32be)
     .  BL.pack
    where
        setBody = do
            indexData <- M.getInput
            let (body, sha1) = BL.splitAt (BL.length indexData - 20) indexData
            if hashlazy body /= BL.toStrict sha1 then
                M.customFailure $ IndexParser "invalid index checksum"
            else
                M.setInput body

lookSignature :: IndexHeader -> ByteStringParser IndexHeader
lookSignature ih
    | ihSignature ih == "DIRC" = pure ih
    | otherwise = M.customFailure $ IndexParser "invalid index signature"

lookVersion :: IndexHeader -> ByteStringParser IndexHeader
lookVersion ih
    | ihVersion ih == 2 = pure ih
    | otherwise = M.customFailure $ IndexParser "unknown index version"

indexBody :: Word32 -> ByteStringParser [IndexEntry]
indexBody expectedEntriesNum = unfoldM (ifM M.atEnd (pure Nothing) idxField)
    >>= lookNumEntries
    where
        idxField = do
            (idxEntryNumConsumed, entry) <- fromBinaryGetter' $ IndexEntry
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
            Just . entry . BL.pack
                <$> M.manyTill M.anySingle pNull >>= padding idxEntryNumConsumed
            where
                -- 1-8 nul bytes as necessary to pad the entry to a multiple of eight bytes
                -- while keeping the name NUL-terminated.
                padding idxEntryNumConsumed path =
                    let numConsumed = fromIntegral idxEntryNumConsumed + length path
                        numPad = (numConsumed + 8) `div` 8 * 8 - numConsumed in
                        path <$ M.skipCount numPad pNull

        lookNumEntries :: [IndexEntry] -> ByteStringParser [IndexEntry]
        lookNumEntries entries
            | length entries /= fromIntegral expectedEntriesNum = M.customFailure
                $ IndexParser
                $ printf "expected number of entries is %d, but got %d" expectedEntriesNum $ length entries
            | otherwise = pure entries

indexParser :: ByteStringParser [IndexEntry]
indexParser = indexHeader
    >>= lookSignature
    >>= lookVersion
    >>= indexBody . ihNumEntries
