module HMGit.Internal.Core.Runner.API (
    HMGitT
  , hmGitDBPath
  , hmGitDBName
  , hmGitRoot
  , hmGitTreeLim
  , getCurrentDirFromHMGit
  , runHMGit
) where

import           HMGit.Internal.Core.Runner.HMGitConfig (HMGitConfig (..))

import           Control.Exception.Safe                 (MonadThrow,
                                                         throwString)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans.Reader             (ReaderT (..), asks)
import           Data.List                              (isPrefixOf)
import           Data.List.Extra                        (dropPrefix)
import qualified Path                                   as P
import qualified Path.IO                                as P
import           System.FilePath                        (takeFileName)
import           Text.Printf                            (printf)

type HMGitT = ReaderT HMGitConfig

hmGitDBPath :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitDBPath = asks hmGitDir

hmGitDBName :: Monad m => HMGitT m String
hmGitDBName = takeFileName . init . P.toFilePath <$> hmGitDBPath

hmGitRoot :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitRoot = P.parent <$> hmGitDBPath

hmGitTreeLim :: Monad m => HMGitT m Int
hmGitTreeLim = asks hmGitTreeLimit

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

runHMGit :: HMGitT m a -> HMGitConfig -> m a
runHMGit = runReaderT

