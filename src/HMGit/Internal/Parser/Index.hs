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
import           Data.Tuple.Extra           (thd3)
import           Data.Word                  (Word16, Word32)
import qualified Text.Megaparsec            as M
import           Text.Printf                (printf)

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

indexHeader :: ByteStringParser Word32
indexHeader = do
    (signature, version, numEntries) <- setBody *> M.count 12 M.anySingle
        <&> BG.runGet ((,,) <$> BG.getLazyByteString 4 <*> BG.getWord32be <*> BG.getWord32be)
         .  BL.pack
    let errMsg = if signature /= "DIRC" then "invalid index signature" else
            if version /= 2 then "unknown index version" else mempty
    if null errMsg then pure numEntries else M.customFailure $ IndexParser errMsg
    where
        setBody = do
            indexData <- M.getInput
            let (body, sha1) = BL.splitAt (BL.length indexData - 20) indexData
            if hashlazy body /= BL.toStrict sha1 then
                M.customFailure $ IndexParser "invalid index checksum"
            else
                M.setInput body

indexBody :: Word32 -> ByteStringParser [IndexEntry]
indexBody expectedEntriesNum = do
    entries <- unfoldM $ ifM M.atEnd (pure Nothing) $ M.getInput <&> BG.runGetOrFail idxEntryField >>= either
        (M.customFailure . IndexParser . thd3)
        (\(unconsumed, _, entry) -> M.setInput unconsumed *> (Just . entry . BL.pack <$> M.manyTill M.anySingle pNull))
    entries <$ entriesExpected expectedEntriesNum entries
    where
        idxEntryField = IndexEntry
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

        entriesExpected :: Word32 -> [IndexEntry] -> ByteStringParser ()
        entriesExpected numEntries entries
            | length entries /= fromIntegral numEntries = M.customFailure
                $ IndexParser
                $ printf "expected number of entries is %d, but got %d" numEntries $ length entries
            | otherwise = pure ()

indexParser :: ByteStringParser [IndexEntry]
indexParser = indexHeader >>= indexBody
