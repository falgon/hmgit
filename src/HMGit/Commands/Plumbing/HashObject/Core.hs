module HMGit.Commands.Plumbing.HashObject.Core (
    HashObjectOpt (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Commands.Plumbing    (Plumbing (..))
import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT,
                                             ObjectInfo (..), fromContents,
                                             liftException, liftIOUnit,
                                             storeObject)
import           HMGit.Internal.Parser      (ObjectType (..))
import           HMGit.Internal.Utils       (formatHexStrings)

import           Control.Exception.Safe     (MonadThrow, throwString)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU
import           Data.Char                  (ord)

newtype HashObjectOpt m = HashObjectOpt (ObjectType -> BL.ByteString -> HMGitT IO (m ()))

instance Plumbing HashObjectOpt where
    runPlumbing (HashObjectOpt f) = f

hashObjectShow :: MonadThrow m => HashObjectOpt m
hashObjectShow = HashObjectOpt $ \objType contents ->
    asks (formatHexStrings . map ord . BC.unpack . objectId . fromContents objType contents . hmGitDir)
        >>= maybe (liftException $ throwString "cannot parse a hex value") (liftIOUnit . putStrLn)

hashObjectWrite :: MonadThrow m => HashObjectOpt m
hashObjectWrite = HashObjectOpt $ \objType contents ->
    storeObject objType contents
        >>= maybe (liftException $ throwString "cannot parse a hex value") (liftIOUnit . putStrLn) .
            formatHexStrings . map ord . BC.unpack

hashObject :: MonadThrow m => HashObjectOpt m -> ObjectType -> FilePath -> HMGitT IO (m ())
hashObject hashObjectOpt objType fpath = BLU.fromString <$> liftIO (readFile fpath)
    >>= runPlumbing hashObjectOpt objType
