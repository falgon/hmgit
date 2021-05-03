module HMGit.Commands.Plumbing.HashObject.Core (
    HashObject (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Commands.Plumbing    (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT,
                                             ObjectInfo (..), fromContents,
                                             storeObject)
import           HMGit.Internal.Parser      (ObjectType (..))
import           HMGit.Internal.Utils       (formatHexByteString')

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU

newtype HashObject m = HashObject (ObjectType -> BL.ByteString -> HMGitT m ())

instance Plumbing HashObject where
    runPlumbing (HashObject f) (PAObject objType body) = f objType body
    runPlumbing _ _                                    = pure ()

hashObjectShow :: (MonadThrow m, MonadIO m) => HashObject m
hashObjectShow = HashObject $ \objType contents ->
    asks (formatHexByteString' . objectId . fromContents objType contents . hmGitDir)
        >>= either throw (liftIO . putStrLn)

hashObjectWrite :: (MonadThrow m, MonadIO m) => HashObject m
hashObjectWrite = HashObject $ \objType contents ->
    storeObject objType contents
        >>= either throw (liftIO . putStrLn) . formatHexByteString'

hashObject :: (MonadThrow m, MonadIO m) => HashObject m -> ObjectType -> FilePath -> HMGitT m ()
hashObject hashObjectOpt objType fpath = BLU.fromString <$> liftIO (readFile fpath)
    >>= runPlumbing hashObjectOpt . PAObject objType
