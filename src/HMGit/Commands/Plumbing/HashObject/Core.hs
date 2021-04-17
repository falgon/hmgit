module HMGit.Commands.Plumbing.HashObject.Core (
    HashObjectOpt (..)
  , hashObjectShow
  , hashObjectWrite
  , hashObject
) where

import           HMGit.Internal.Core        (HMGitConfig (..), HMGitT,
                                             ObjectInfo (..), fromContents,
                                             storeObject)
import           HMGit.Internal.Parser      (ObjectType (..))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU

newtype HashObjectOpt = HashObjectOpt {
    runHashObject :: ObjectType -> BL.ByteString -> HMGitT IO ()
  }

hashObjectShow :: HashObjectOpt
hashObjectShow = HashObjectOpt $ \objType contents ->
    asks (objectId . fromContents objType contents . hmGitDir)
        >>= liftIO . BC.putStrLn

hashObjectWrite :: HashObjectOpt
hashObjectWrite = HashObjectOpt $ \objType contents ->
    storeObject objType contents >>= liftIO . BC.putStrLn

hashObject :: HashObjectOpt -> ObjectType -> FilePath -> HMGitT IO ()
hashObject hashObjectOpt objType fpath = BLU.fromString <$> liftIO (readFile fpath)
    >>= runHashObject hashObjectOpt objType
