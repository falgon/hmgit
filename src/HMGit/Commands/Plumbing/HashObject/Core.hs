module HMGit.Commands.Plumbing.HashObject.Core (
    HashObject (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Internal.Core       (HMGitT, ObjectInfo (..),
                                            fromContents, storeObject)
import           HMGit.Internal.Parser     (ObjectType (..))
import           HMGit.Internal.Utils      (formatHexByteString')

import           Control.Exception.Safe    (MonadCatch, throw)
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU

newtype HashObject m = HashObject (ObjectType -> BL.ByteString -> HMGitT m ())

hashObjectShow :: (MonadCatch m, MonadIO m) => HashObject m
hashObjectShow = HashObject $ \objType contents ->
    fromContents objType contents
        >>= either throw (liftIO . putStrLn) . formatHexByteString' . objectId

hashObjectWrite :: (MonadCatch m, MonadIO m) => HashObject m
hashObjectWrite = HashObject $ \objType contents ->
    storeObject objType contents
        >>= either throw (liftIO . putStrLn) . formatHexByteString'

hashObject :: (MonadCatch m, MonadIO m)
    => HashObject m
    -> ObjectType
    -> FilePath
    -> HMGitT m ()
hashObject (HashObject f) objType fpath = liftIO (readFile fpath)
    >>= f objType . BLU.fromString
