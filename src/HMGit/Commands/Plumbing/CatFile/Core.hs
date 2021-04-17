{-# LANGUAGE CPP, LambdaCase, OverloadedStrings #-}
module HMGit.Commands.Plumbing.CatFile.Core (
    CatOpt (..)
  , catOptObject
  , catOptObjectType
  , catOptObjectSize
  , catOptObjectPP
  , catFile
) where

import           HMGit.Commands.Plumbing    (Plumbing (..))
import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT,
                                             loadObject, loadTreeFromData)
import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (asks)
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

data CatOpt = CatOptObjectType ObjectType (ObjectType -> BL.ByteString -> HMGitT IO ())
    | CatOptMode (ObjectType -> BL.ByteString -> HMGitT IO ())

instance Plumbing CatOpt where
    runPlumbing (CatOptObjectType specifiedObjType f) objType body
        | specifiedObjType /= objType = liftIO $ pure $ throw $ invalidArgument $ unwords [
            "expected object type"
            , show specifiedObjType <> ","
            , "but got"
            , show objType
            ]
        | otherwise = pure () <$ f objType body
    runPlumbing (CatOptMode f) objType body = pure () <$ f objType body

catOptObject :: ObjectType -> CatOpt
catOptObject = flip CatOptObjectType $ const (liftIO . BLC.putStrLn)

catOptObjectType :: CatOpt
catOptObjectType = CatOptMode $ const . liftIO . print

catOptObjectSize :: CatOpt
catOptObjectSize = CatOptMode $ const $ liftIO . print . BL.length

catOptObjectPP :: CatOpt
catOptObjectPP = CatOptMode $ \objType body -> if objType `elem` [ Commit, Blob ] then liftIO (BLC.putStrLn body) else
    asks (loadTreeFromData body . hmGitTreeLimit) >>= \case
        Left err -> liftIO $ hPrint stderr err
        Right objs -> liftIO $ forM_ objs $ \(mode, fpath, sha1) -> putStrLn $ unwords [
            showOct mode ""
          , if sIsDir mode then "tree" else "blob"
          , sha1
          ] <> "\t" <> fpath

catFile :: MonadThrow m => CatOpt -> B.ByteString -> HMGitT IO (m ())
catFile catOpt sha1 = loadObject sha1
    >>= either (liftIO . pure . throw) (uncurry (runPlumbing catOpt))
