{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module HMGit.Internal.Core (
    HMGitConfig (..)
  , defaultHMGitConfig
  , HMGitT
  , runHMGit
  , liftIOUnit
  , liftException
  , ObjectInfo (..)
  , fromContents
  , storeObject
  , loadObject
  , loadTreeFromData
  , loadIndex
) where

import           HMGit.Internal.Exceptions
import           HMGit.Internal.Parser      (IndexEntry, ObjectType (..),
                                             indexParser, objectParser,
                                             treeParser)
import           HMGit.Internal.Utils       (stateEmpty)

import           Codec.Compression.Zlib     (compress, decompress)
import           Control.Exception.Safe     (MonadThrow, throw)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..), asks)
import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU
import qualified Data.ByteString.UTF8       as BU
import           Data.Functor               (($>), (<&>))
import           Data.List                  (intercalate, isPrefixOf)
import qualified Data.List.NonEmpty         as LN
import           Data.Tuple.Extra           (first)
import           Prelude                    hiding (init)
import           System.Directory           (createDirectoryIfMissing,
                                             listDirectory)
import           System.FilePath            ((</>))
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M

data HMGitConfig = HMGitConfig {
    hmGitDir       :: FilePath -> FilePath
  , hmGitTreeLimit :: Int
  }

defaultHMGitConfig :: HMGitConfig
defaultHMGitConfig = HMGitConfig {
    hmGitDir = (</> ".hmgit")
  , hmGitTreeLimit = 1000
  }

type HMGitT = ReaderT HMGitConfig

runHMGit :: HMGitT m a -> HMGitConfig -> m a
runHMGit = runReaderT

liftIOUnit :: (MonadIO m, Applicative f) => IO a -> m (f a)
liftIOUnit = liftIO . fmap pure

liftException :: (MonadIO m, Applicative f) => f a -> m (f a)
liftException = liftIO . pure

data ObjectInfo = ObjectInfo {
    objectId   :: BU.ByteString
  , objectData :: BL.ByteString
  , objectPath :: (FilePath, FilePath)
  }


objectFormat :: ObjectType -> BL.ByteString -> BL.ByteString
objectFormat objType contents = mconcat [
    BLU.fromString $ show objType
  , " "
  , BLU.fromString $ show $ BL.length contents
  , BL.singleton 0
  , contents
  ]

hashToPath :: B.ByteString -> (FilePath -> FilePath) -> (FilePath, FilePath)
hashToPath sha1 hgd = (
    intercalate "/" [ hgd ".", "objects", BU.toString $ B.take 2 sha1 ]
  , BU.toString $ B.drop 2 sha1
  )

fromContents :: ObjectType -> BL.ByteString -> (FilePath -> FilePath) -> ObjectInfo
fromContents objType contents hgd = ObjectInfo {
    objectId = objId
  , objectData = compress objFormat
  , objectPath = hashToPath objId hgd
  }
    where
        objFormat = objectFormat objType contents
        objId = hashlazy objFormat

storeObject :: ObjectType -> BL.ByteString -> HMGitT IO B.ByteString
storeObject objType contents = do
    objInfo <- asks $ fromContents objType contents . hmGitDir
    lift (createDirectoryIfMissing True (fst $ objectPath objInfo))
        *> lift (BL.writeFile (uncurry (</>) $ objectPath objInfo) (objectData objInfo))
        $> objectId objInfo

loadObject :: MonadThrow m => B.ByteString -> HMGitT IO (m (ObjectType, BL.ByteString))
loadObject sha1
    | B.length sha1 < 2 = liftIO $ pure $ throw $ invalidArgument "hash prefix must be 2 or more characters"
    | otherwise = runMaybeT loadObject' >>= \case
        Nothing -> liftIO $ throw $ nosuchThing $ unwords [
            "objects"
          , BU.toString sha1
          , "not found or multiple object ("
          , show $ B.length sha1
          , ") with prefix"
          , BU.toString sha1
          ]
        Just (fname, object) -> case M.runParser objectParser fname object of
            Left errorBundle      -> liftException $ throw errorBundle
            Right (objType, body) -> liftIOUnit $ pure (objType, body)
    where
        loadObject' = do
            (dir, rest) <- lift $ asks $ hashToPath sha1 . hmGitDir
            fname <- MaybeT (LN.nonEmpty . filter (isPrefixOf rest) <$> lift (listDirectory dir))
                >>= stateEmpty . first head . LN.splitAt 1
                <&> (dir </>)
            (fname,) <$> lift (lift (decompress <$> BL.readFile fname))

loadTreeFromData :: MonadThrow m => BL.ByteString -> Int -> m [(CMode, FilePath, String)]
loadTreeFromData body treeLimit = either throw pure $ M.runParser (treeParser treeLimit) mempty body

loadIndex :: MonadThrow m => HMGitT IO (m [IndexEntry])
loadIndex = do
    fname <- asks ((</> "index") . flip id "." . hmGitDir)
    M.runParser indexParser fname <$> liftIO (BL.readFile fname)
        >>= either (liftException . throw) (liftIOUnit . pure)

