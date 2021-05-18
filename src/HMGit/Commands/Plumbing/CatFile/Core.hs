{-# LANGUAGE CPP, OverloadedStrings #-}
module HMGit.Commands.Plumbing.CatFile.Core (
    CatFile (..)
  , catOptObject
  , catOptObjectType
  , catOptObjectSize
  , catOptObjectPP
  , catFile
) where

import           HMGit.Internal.Core        (HMGitT, loadObject, loadTree)
import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser
import           Text.Printf                (printf)

import           Control.Exception.Safe     (MonadCatch, MonadThrow, throw)
import           Control.Monad              (MonadPlus)
import           Control.Monad.IO.Class     (MonadIO (..))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.String                (IsString (..))
import qualified Path                       as P
import           Prelude                    hiding (init)
import           System.Posix.Types         (CMode (..))
#ifndef mingw32_HOST_OS
import           System.Posix.Internals     (s_isdir)
sIsDir :: CMode -> Bool
sIsDir = s_isdir
#else
{-
import           Data.Bits                  ((.&.))
sIsDir :: CMode -> Bool
sIsDir = (== sIFDIR) . (.&. sIFMT)
    where
        sIFMT = 0o170000
        sIFDIR = 0o040000 -}
#endif

data CatFile m = CatFileObjectType ObjectType (ObjectType -> BL.ByteString -> HMGitT m ())
    | CatFileMode (ObjectType -> BL.ByteString -> HMGitT m ())

instance MonadIO m => IsString (CatFile m) where
    fromString = catOptObject . read

catOptObject :: MonadIO m => ObjectType -> CatFile m
catOptObject = flip CatFileObjectType $ const (liftIO . BLC.putStr)

catOptObjectType :: MonadIO m => CatFile m
catOptObjectType = CatFileMode $ const . liftIO . print

catOptObjectSize :: MonadIO m => CatFile m
catOptObjectSize = CatFileMode $ const $ liftIO . print . BL.length

catOptObjectPP :: (MonadIO m, MonadThrow m) => CatFile m
catOptObjectPP = CatFileMode $ \objType body ->
    if objType `elem` [ Commit, Blob ] then liftIO (BLC.putStr body)
    else loadTree body
        >>= mapM_ (\(mode, fpath, sha1) ->
            liftIO $ putStrLn $ unwords [
                printf "%06o" (fromIntegral mode :: Integer)
              , if sIsDir mode then "tree" else "blob"
              , sha1
              ] <> printf "\t%s" (P.toFilePath fpath))

catFile :: (MonadIO m, MonadCatch m, MonadPlus m)
    => CatFile m
    -> B.ByteString
    -> HMGitT m ()
catFile catOpt sha1 = do
    (objType, body) <- loadObject sha1
    case catOpt of
        CatFileObjectType specifiedObjType f
            | specifiedObjType /= objType -> throw
                $ invalidArgument
                $ unwords [
                    "expected object type"
                  , show specifiedObjType <> ","
                  , "but got"
                  , show objType
                  ]
            | otherwise -> f objType body
        CatFileMode f -> f objType body
