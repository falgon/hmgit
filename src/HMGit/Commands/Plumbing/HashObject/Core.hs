module HMGit.Commands.Plumbing.HashObject.Core (
    HashObjectOpt (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Commands.Plumbing    (Plumbing (..), PlumbingArgs (..))
import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT,
                                             ObjectInfo (..), fromContents,
                                             liftException, liftIOUnit,
                                             storeObject)
import           HMGit.Internal.Parser      (ObjectType (..))
import           HMGit.Internal.Utils       (formatHexByteString')

import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU

newtype HashObjectOpt m = HashObjectOpt (ObjectType -> BL.ByteString -> HMGitT IO (m ()))

instance Plumbing HashObjectOpt where
    runPlumbing (HashObjectOpt f) (PAObject objType body) = f objType body
    runPlumbing _ _ = liftIOUnit $ pure ()

hashObjectShow :: MonadThrow m => HashObjectOpt m
hashObjectShow = HashObjectOpt $ \objType contents ->
    asks (formatHexByteString' . objectId . fromContents objType contents . hmGitDir)
        >>= either (liftException . throw) (liftIOUnit . putStrLn)

hashObjectWrite :: MonadThrow m => HashObjectOpt m
hashObjectWrite = HashObjectOpt $ \objType contents ->
    storeObject objType contents
        >>= either (liftException . throw) (liftIOUnit . putStrLn) . formatHexByteString'

hashObject :: MonadThrow m => HashObjectOpt m -> ObjectType -> FilePath -> HMGitT IO (m ())
hashObject hashObjectOpt objType fpath = BLU.fromString <$> liftIO (readFile fpath)
    >>= runPlumbing hashObjectOpt . PAObject objType
