{-# LANGUAGE CPP, LambdaCase, OverloadedStrings #-}
module HMGit.Commands.Plumbing.CatFile.Core (
    CatOpt (..)
  , catOptObject
  , catOptObjectType
  , catOptObjectSize
  , catOptObjectPP
  , catFile
) where

import           HMGit.Commands.Plumbing    (Plumbing (..), PlumbingArgs (..))
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

data CatOpt m = CatOptObjectType ObjectType (ObjectType -> BL.ByteString -> HMGitT m ())
    | CatOptMode (ObjectType -> BL.ByteString -> HMGitT m ())

instance Plumbing CatOpt where
    runPlumbing (CatOptObjectType specifiedObjType f) (PAObject objType body)
        | specifiedObjType /= objType = throw $ invalidArgument $ unwords [
            "expected object type"
            , show specifiedObjType <> ","
            , "but got"
            , show objType
            ]
        | otherwise = f objType body
    runPlumbing (CatOptMode f) (PAObject objType body) = f objType body
    runPlumbing _ _ = pure ()

catOptObject :: MonadIO m => ObjectType -> CatOpt m
catOptObject = flip CatOptObjectType $ const (liftIO . BLC.putStrLn)

catOptObjectType :: MonadIO m => CatOpt m
catOptObjectType = CatOptMode $ const . liftIO . print

catOptObjectSize :: MonadIO m => CatOpt m
catOptObjectSize = CatOptMode $ const $ liftIO . print . BL.length

catOptObjectPP :: (MonadIO m, MonadThrow m) => CatOpt m
catOptObjectPP = CatOptMode $ \objType body -> if objType `elem` [ Commit, Blob ] then
    liftIO (BLC.putStrLn body) else
        asks (loadTreeFromData body . hmGitTreeLimit) >>= \case
            Left err -> throw err
            Right objs -> forM_ objs $ \(mode, fpath, sha1) ->
                liftIO $ putStrLn $ unwords [
                    showOct mode mempty
                  , if sIsDir mode then "tree" else "blob"
                  , sha1
                  ] <> "\t" <> fpath

catFile :: (MonadIO m, MonadThrow m) => CatOpt m -> B.ByteString -> HMGitT m ()
catFile catOpt sha1 = loadObject sha1
    >>= runPlumbing catOpt . uncurry PAObject
