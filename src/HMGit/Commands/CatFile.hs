{-# LANGUAGE CPP, OverloadedStrings #-}
module HMGit.Commands.CatFile (
    CatOpt (..)
  , catOptObject
  , catOptObjectType
  , catOptObjectSize
  , catOptObjectPP
  , catFile
) where

import           HMGit.Internals            (loadObject, loadTreeFromData)
import           HMGit.Parser

import           Control.Monad              (forM_)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Numeric                    (showOct)
import           Prelude                    hiding (init)
import           System.IO                  (hPrint, hPutStrLn, stderr)
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
          ] <> concat [
            "\t"
          , fpath
          ]

runCatOpt :: CatOpt -> ObjectType -> BL.ByteString -> IO ()
runCatOpt (CatOptObjectType specifiedObjType runner) objType body
    | specifiedObjType /= objType = hPutStrLn stderr $ unwords [
        "expected object type"
        , show specifiedObjType <> ","
        , "but got"
        , show objType
        ]
    | otherwise = runner objType body
runCatOpt catOptMode objType body = runCatOptMode catOptMode objType body

catFile :: CatOpt -> B.ByteString -> IO ()
catFile catOpt sha1 = loadObject sha1
    >>= either (hPrint stderr) (uncurry (runCatOpt catOpt))
