module HMGit.Commands.Plumbing.HashObject.Core (
    HashObject (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Internal.Core        (ObjectInfo (..), fromContents,
                                             storeObject)
import           HMGit.Internal.Core.Runner (HMGitT)
import           HMGit.Internal.Parser      (ObjectType (..))
import           HMGit.Internal.Utils       (hexStr)

import           Control.Exception.Safe     (MonadCatch)
import           Control.Monad              ((>=>))
import           Control.Monad.IO.Class     (MonadIO (..))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU

newtype HashObject m = HashObject (ObjectType -> BL.ByteString -> HMGitT m ())

hashObjectShow :: (MonadCatch m, MonadIO m)
    => HashObject m
hashObjectShow = HashObject $ \objType ->
    fromContents objType
        >=> liftIO . putStrLn . hexStr . objectId

hashObjectWrite :: (MonadCatch m, MonadIO m)
    => HashObject m
hashObjectWrite = HashObject $ \objType ->
    storeObject objType
        >=> liftIO . putStrLn . hexStr

hashObject :: (MonadCatch m, MonadIO m)
    => HashObject m
    -> ObjectType
    -> FilePath
    -> HMGitT m ()
hashObject (HashObject f) objType fpath =
    liftIO (readFile fpath)
        >>= f objType . BLU.fromString
