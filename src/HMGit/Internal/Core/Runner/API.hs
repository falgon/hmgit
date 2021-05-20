{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module HMGit.Internal.Core.Runner.API (
    HMGitT
  , hmGitDBPath
  , hmGitDBName
  , hmGitLoadMasterHash
  , hmGitRoot
  , hmGitTreeLim
  , hmGitIndexPath
  , hmGitBRPath
  , hmGitBRPath'
  , hmGitBRName
  , hmGitBRName'
  , getCurrentDirFromHMGit
  , runHMGit
) where

import           HMGit.Internal.Core.Runner.HMGitConfig (HMGitConfig (..))
import           HMGit.Internal.Parser.Core.ByteString  (ParseException (..))

import qualified Codec.Binary.UTF8.String               as BUS
import           Control.Arrow                          ((|||))
import           Control.Exception.Safe                 (MonadThrow, throw,
                                                         throwString)
import           Control.Monad.Extra                    (ifM, replicateM)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans                    (lift)
import           Control.Monad.Trans.Reader             (ReaderT (..), asks)
import qualified Data.Binary.Get                        as BG
import qualified Data.ByteString.Lazy                   as BL
import           Data.Functor                           ((<&>))
import           Data.List                              (isPrefixOf)
import           Data.List.Extra                        (dropPrefix)
import           Data.Tuple.Extra                       (thd3, third3)
import           Path                                   (File, Rel)
import qualified Path                                   as P
import qualified Path.IO                                as P
import           System.FilePath                        (takeFileName)
import           Text.Printf                            (printf)

type HMGitT = ReaderT HMGitConfig

-- | Get an abstract path to the HMGit database directory
hmGitDBPath :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitDBPath = asks hmGitDir

-- | Get the HMGit database name
hmGitDBName :: Monad m => HMGitT m String
hmGitDBName = takeFileName . init . P.toFilePath <$> hmGitDBPath

-- | Get an abstract path to the HMGit project directory
hmGitRoot :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitRoot = P.parent <$> hmGitDBPath

-- | Get a loop limit of the tree
hmGitTreeLim :: Monad m => HMGitT m Int
hmGitTreeLim = asks hmGitTreeLimit

-- | The index file path
hmGitIndexPath :: Monad m => HMGitT m (P.Path P.Abs P.File)
hmGitIndexPath = (P.</> $(P.mkRelFile "index")) <$> hmGitDBPath

-- | Returns the current path relative to the hmgit database directory
getCurrentDirFromHMGit :: (MonadThrow m, MonadIO m)
    => HMGitT m (P.Path P.Rel P.Dir)
getCurrentDirFromHMGit = do
    currentDir <- P.toFilePath <$> P.getCurrentDir
    rootPath <- P.toFilePath <$> hmGitRoot
    if rootPath `isPrefixOf` currentDir then
        let path = dropPrefix rootPath currentDir in
            P.parseRelDir $ if null path then "./" else path
    else
        hmGitDBName
            >>= throwString
             . printf "The current working directory is not in %s repository"

-- | If @refs\/heads\/master@ or @refs\/heads\/main@ exists, return @Right@,
-- if they are not exist, the candidate path is wrapped in @Left@ returned.
hmGitBRPath :: MonadIO m => HMGitT m (Either (P.Path P.Abs P.File) (P.Path P.Abs P.File))
hmGitBRPath = do
    m <- hmGitDBPath <&> (P.</> $(P.mkRelFile "refs/heads/master"))
    ifM (P.doesFileExist m) (lift $ pure $ Right m) $ do
        m' <- hmGitDBPath <&> (P.</> $(P.mkRelFile "refs/heads/main"))
        ifM (P.doesFileExist m') (lift $ pure $ Right m') $
            lift $ pure $ Left m'

-- | Get the path, ignoring whether it is an actual path or a candidate path.
hmGitBRPath' :: MonadIO m => HMGitT m (P.Path P.Abs P.File)
hmGitBRPath' = hmGitBRPath >>= pure ||| pure

-- | If @refs\/heads\/master@ or @refs\/heads\/main@ exists, return @Right@,
-- if they are not exist, the candidate name is wrapped in @Left@ returned.
hmGitBRName :: MonadIO m => HMGitT m (Either String String)
hmGitBRName = hmGitBRPath
    <&> either (Left . P.toFilePath . P.filename) (Right . P.toFilePath . P.filename)

-- | Get the name, ignoring whether it is a real name or a candidate name.
hmGitBRName' :: MonadIO m => HMGitT m String
hmGitBRName' = hmGitBRName >>= pure ||| pure

-- | Read refs\/heads\/master. If it does not exist,
-- look for refs\/heads\/main as a fallback.
-- The @Nothing@ will be returned if neither exists.
hmGitLoadMasterHash :: (MonadIO m, MonadThrow m) => HMGitT m (Maybe String)
hmGitLoadMasterHash = hmGitBRPath
    >>= either (const $ pure Nothing) readHash
    where
        readHash fpath = do
            (uc, _, fp) <- liftIO (BL.readFile $ P.toFilePath fpath)
                >>= either (throw . MasterHashParser . thd3) pure
                    . BG.runGetOrFail (replicateM 40 BG.getWord8)
                <&> third3 BUS.decode
            if BL.null uc || uc == "\n" then pure $ Just fp else
                lift $ throw $ MasterHashParser "invalid master/main hash"

-- | HMGit runner
runHMGit :: HMGitT m a -> HMGitConfig -> m a
runHMGit = runReaderT

