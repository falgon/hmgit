{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module HMGit.Internal.Core (
    HMGitConfig (..)
  , getHMGitPath
  , HMGitT
  , hmGitRoot
  , getCurrentDirectoryFromHMGit
  , runHMGit
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
import           Control.Exception.Safe     (MonadThrow, throw, throwString)
import           Control.Monad.Extra        (andM, ifM)
import           Control.Monad.Fix          (fix)
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
import           Data.List.Extra            (dropPrefix)
import qualified Data.List.NonEmpty         as LN
import           Data.Tuple.Extra           (first)
import           Data.Void                  (Void)
import           Prelude                    hiding (init)
import           System.Directory           (canonicalizePath,
                                             createDirectoryIfMissing,
                                             doesDirectoryExist,
                                             getCurrentDirectory, listDirectory)
import           System.FilePath            (takeDirectory, (</>))
import           System.Posix.Types         (CMode (..))
import qualified Text.Megaparsec            as M

data HMGitConfig = HMGitConfig {
    hmGitDir       :: FilePath
  , hmGitTreeLimit :: Int
  }

isHMGitDir :: MonadIO m => FilePath -> m Bool
isHMGitDir fp = liftIO $ andM [
    doesDirectoryExist fp
  , doesDirectoryExist $ fp </> "objects"
  , doesDirectoryExist $ fp </> "refs"
  ]

getHMGitPath :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
getHMGitPath hmGitDirName = do
    currentDirectory <- liftIO getCurrentDirectory
    ($ currentDirectory) . fix $ \f cwd -> ifM (not <$> liftIO (doesDirectoryExist cwd)) (throw $ noSuchThing errMsg) $
        let expectedHMGitDirPath = cwd </> hmGitDirName in ifM (isHMGitDir expectedHMGitDirPath)
            (liftIO $ canonicalizePath expectedHMGitDirPath)
          $ f (cwd </> "..")
    where
        errMsg = "not a git repository (or any of the parent directories): .git"

type HMGitT = ReaderT HMGitConfig

hmGitRoot :: Monad m => HMGitT m FilePath
hmGitRoot = asks (takeDirectory . hmGitDir)

getCurrentDirectoryFromHMGit :: (MonadThrow m, MonadIO m) => HMGitT m FilePath
getCurrentDirectoryFromHMGit = do
    currentDirectory <- liftIO getCurrentDirectory
    rootPath <- hmGitRoot
    if rootPath `isPrefixOf` currentDirectory then
        pure $ let p = dropPrefix rootPath currentDirectory in
            if null p then p else tail p <> "/"
    else
        throwString "The current working directory is not in hmgit repository"

runHMGit :: HMGitT m a -> HMGitConfig -> m a
runHMGit = runReaderT

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

hashToPath :: B.ByteString -> FilePath -> (FilePath, FilePath)
hashToPath sha1 hgd = (
    intercalate "/" [ hgd, "objects", BU.toString $ B.take 2 sha1 ]
  , BU.toString $ B.drop 2 sha1
  )

fromContents :: ObjectType -> BL.ByteString -> FilePath -> ObjectInfo
fromContents objType contents hgd = ObjectInfo {
    objectId = objId
  , objectData = compress objFormat
  , objectPath = hashToPath objId hgd
  }
    where
        objFormat = objectFormat objType contents
        objId = hashlazy objFormat

storeObject :: MonadIO m => ObjectType -> BL.ByteString -> HMGitT m B.ByteString
storeObject objType contents = do
    objInfo <- asks $ fromContents objType contents . hmGitDir
    liftIO (createDirectoryIfMissing True (fst $ objectPath objInfo))
        *> liftIO (BL.writeFile (uncurry (</>) $ objectPath objInfo) (objectData objInfo))
        $> objectId objInfo

loadObject :: (MonadIO m, MonadThrow m) => B.ByteString -> HMGitT m (ObjectType, BL.ByteString)
loadObject sha1
    | B.length sha1 < 2 = lift $ throw $ invalidArgument "hash prefix must be 2 or more characters"
    | otherwise = runMaybeT loadObject' >>= \case
        Nothing -> lift $ throw $ noSuchThing $ unwords [
            "objects"
          , BU.toString sha1
          , "not found or multiple object ("
          , show $ B.length sha1
          , ") with prefix"
          , BU.toString sha1
          ]
        Just (fname, object) -> case M.runParser objectParser fname object of
            Left errorBundle      -> lift $ throw errorBundle
            Right (objType, body) -> lift $ pure (objType, body)
    where
        loadObject' = do
            (dir, rest) <- lift $ asks $ hashToPath sha1 . hmGitDir
            fname <- MaybeT (LN.nonEmpty . filter (isPrefixOf rest) <$> lift (liftIO (listDirectory dir)))
                >>= stateEmpty . first head . LN.splitAt 1
                <&> (dir </>)
            (fname,) <$> lift (lift (decompress <$> liftIO (BL.readFile fname)))

loadTreeFromData :: MonadThrow m => BL.ByteString -> Int -> m [(CMode, FilePath, String)]
loadTreeFromData body treeLimit = either throw pure $ M.runParser (treeParser treeLimit) mempty body

loadIndex :: (MonadIO m, MonadThrow m) => HMGitT m [IndexEntry]
loadIndex = do
    fname <- asks $ (</> "index") . hmGitDir
    liftIO (BL.readFile fname)
        <&> M.runParser indexParser fname
        >>= fromMonad (Nothing :: Maybe Void)

