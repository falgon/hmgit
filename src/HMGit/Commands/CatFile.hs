{-# LANGUAGE CPP, OverloadedStrings #-}
module HMGit.Commands.CatFile (
    CatOpt (..)
  , catOptObject
  , catOptObjectType
  , catOptObjectSize
  , catOptObjectPP
  , catFile
) where

import           HMGit.Internal.Core        (loadObject, loadTreeFromData)
import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad              (forM_)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Numeric                    (showOct)
import           Prelude                    hiding (init)
import           System.IO                  (hPrint, stderr)
import           System.Posix.Types         (CMode (..))
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

data CatOpt = CatOptObjectType ObjectType (ObjectType -> BL.ByteString -> IO ())
    | CatOptMode { runCatOptMode :: ObjectType -> BL.ByteString -> IO () }

catOptObject :: ObjectType -> CatOpt
catOptObject = flip CatOptObjectType $ const BLC.putStrLn

catOptObjectType :: CatOpt
catOptObjectType = CatOptMode $ const . print

catOptObjectSize :: CatOpt
catOptObjectSize = CatOptMode $ const $ print . BL.length

catOptObjectPP :: CatOpt
catOptObjectPP = CatOptMode $ \objType body ->
    if objType `elem` [ Commit, Blob ] then BLC.putStrLn body else case loadTreeFromData body of
        Left err -> hPrint stderr err
        Right objs -> forM_ objs $ \(mode, fpath, sha1) -> putStrLn $ unwords [
            showOct mode ""
          , if sIsDir mode then "tree" else "blob"
          , sha1
          ] <> "\t" <> fpath

runCatOpt :: MonadThrow m => CatOpt -> ObjectType -> BL.ByteString -> IO (m ())
runCatOpt (CatOptObjectType specifiedObjType runner) objType body
    | specifiedObjType /= objType = pure $ throw $ invalidArgument $ unwords [
        "expected object type"
        , show specifiedObjType <> ","
        , "but got"
        , show objType
        ]
    | otherwise = pure () <$ runner objType body
runCatOpt catOptMode objType body = pure () <$ runCatOptMode catOptMode objType body

catFile :: MonadThrow m => CatOpt -> B.ByteString -> IO (m ())
catFile catOpt sha1 = loadObject sha1
    >>= either (pure . throw) (uncurry (runCatOpt catOpt))
