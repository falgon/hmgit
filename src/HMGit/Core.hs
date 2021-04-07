{-# LANGUAGE CPP, LambdaCase, OverloadedStrings, TupleSections #-}
module HMGit.Core (
    storeObject
  , loadObject
) where

import           HMGit.Parser

import           Codec.Compression.Zlib     (compress, decompress)
import           Control.Exception.Safe     (MonadThrow (..), SomeException,
                                             throwString)
import           Control.Monad              (MonadPlus (..), forM_)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8  as BLU
import qualified Data.ByteString.UTF8       as BU
import           Data.List                  (intercalate, isPrefixOf, unwords)
import qualified Data.List.NonEmpty         as LN
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
import           Prelude                    hiding (init)
import           System.Directory           (createDirectoryIfMissing,
                                             listDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (hPrint, hPutStrLn, stderr)
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Error      as M

#ifndef mingw32_HOST_OS
import           System.Posix.Internals     (s_isdir)

sIsDir :: CMode -> Bool
sIsDir = s_isdir
#else
import           Data.Bits                  ((.&.))

sIsDir :: CMode -> Bool
sIsDir = (== sIFDIR) . (.&. sIFMT)
    where
        sIFMT = 0o170000
        sIFDIR = 0o040000
#endif

hmGitDir :: FilePath -> FilePath
hmGitDir = flip (</>) ".hmgit"

hmGitInitDir :: FilePath -> [FilePath]
hmGitInitDir repoName = map (hmGitDir repoName </>) [
    "objects"
  , "refs" </> "heads"
  ]

init :: FilePath -> IO ()
init repoName = mapM_ (createDirectoryIfMissing True) (hmGitInitDir repoName)
    *> BL.writeFile (hmGitDir repoName </> "HEAD") "ref: refs/heads/master"

hashByteData :: BL.ByteString -> ObjectType -> BL.ByteString
hashByteData rawData objType = mconcat [
    BLU.fromString $ show objType
  , " "
  , BLU.fromString $ show $ BL.length rawData
  , BL.singleton 0
  , rawData
  ]

hashObject :: BL.ByteString -> ObjectType -> B.ByteString
hashObject = (.) hashlazy . hashByteData

hashToPath :: B.ByteString -> (FilePath, FilePath)
hashToPath sha1 = (
    intercalate "/" [ hmGitDir ".", "objects", BU.toString $ B.take 2 sha1 ]
  , BU.toString $ B.drop 2 sha1
  )

storeObject :: ObjectType -> BL.ByteString -> IO B.ByteString
storeObject objType rawData = sha1 <$
    (createDirectoryIfMissing True (fst path) *> BL.writeFile (uncurry (</>) path) (compress fullData))
    where
        fullData = hashByteData rawData objType
        sha1 = hashlazy fullData
        path = hashToPath sha1

loadObject :: MonadThrow m => B.ByteString -> IO (m (ObjectType, BL.ByteString))
loadObject sha1
    | B.length sha1 < 1 = pure $ throwString "hash prefix must be 2 or more characters"
    | otherwise = runMaybeT getValidObject >>= \case
        Nothing -> throwString $ unwords [
            "objects"
          , BU.toString sha1
          , "not found or multiple object ("
          , show $ B.length sha1
          , ") with prefix"
          , BU.toString sha1
          ]
        Just (fname, object) -> case M.runParser objectParser fname object of
            Left errorBundle -> throwString $ M.errorBundlePretty errorBundle
            Right (objType, body) -> pure $ pure (objType, body)
    where
        getValidObject = do
            let (dir, rest) = hashToPath sha1
            (object, ext) <- LN.splitAt 1 <$> MaybeT (LN.nonEmpty . filter (isPrefixOf rest) <$> listDirectory dir)
            if not $ null ext then mzero else let fname = dir </> head object in
                (fname,) <$> lift (decompress <$> BL.readFile fname)

loadTreeFromData :: BL.ByteString -> Either (M.ParseErrorBundle BLU.ByteString Void) [(CMode, FilePath, BL.ByteString)]
loadTreeFromData = M.runParser (treeParser 1000) mempty

data CatOpt = CatOptObjectType ObjectType (ObjectType -> BL.ByteString -> IO ())
    | CatOptMode { runCatOptMode :: ObjectType -> BL.ByteString -> IO () }

catObjectO :: ObjectType -> CatOpt
catObjectO = flip CatOptObjectType $ const BLC.putStrLn

catObjectType :: CatOpt
catObjectType = CatOptMode $ const . print

catObjectSize :: CatOpt
catObjectSize = CatOptMode $ const $ print . BL.length

catObjectPP :: CatOpt
catObjectPP = CatOptMode $ \objType body ->
    if objType `elem` [ Commit, Blob ] then BLC.putStrLn body else case loadTreeFromData body of
        Left err -> hPutStrLn stderr $ M.errorBundlePretty err
        Right objs -> forM_ objs $ \(mode, fpath, sha1) -> putStrLn $ unwords [
            show mode -- todo octal display
          , if sIsDir mode then "tree" else "blob"
          , BLU.toString sha1
          , fpath
          ]

runCatOpt :: CatOpt -> ObjectType -> BL.ByteString -> IO ()
runCatOpt (CatOptObjectType specifiedObjType runner) objType body
    | specifiedObjType /= objType = hPutStrLn stderr $ unwords [
        "expected objType type"
        , show specifiedObjType
        , "got"
        , show objType
        ]
    | otherwise = runner objType body
runCatOpt catOptMode objType body = runCatOptMode catOptMode objType body

catObject :: CatOpt -> B.ByteString -> IO ()
catObject catOpt sha1 = loadObject sha1
    >>= either (hPrint stderr) (uncurry (runCatOpt catOpt))
