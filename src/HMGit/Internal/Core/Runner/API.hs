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
import           HMGit.Internal.Exceptions              (MonadThrowable (..))

import           Control.Exception.Safe                 (MonadThrow,
                                                         throwString)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans.Reader             (ReaderT (..), asks)
import           Data.List                              (isPrefixOf)
import           Data.List.Extra                        (dropPrefix)
import qualified Data.Text.IO                           as T
import           Data.Void                              (Void)
import qualified Path                                   as P
import qualified Path.IO                                as P
import           System.FilePath                        (takeFileName)
import           Text.Printf                            (printf)
import qualified Text.Toml                              as Toml

type HMGitT = ReaderT HMGitConfig

hmGitDBPath :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitDBPath = asks hmGitDir

hmGitDBName :: Monad m => HMGitT m String
hmGitDBName = takeFileName . init . P.toFilePath <$> hmGitDBPath

hmGitRoot :: Monad m => HMGitT m (P.Path P.Abs P.Dir)
hmGitRoot = P.parent <$> hmGitDBPath

hmGitTreeLim :: Monad m => HMGitT m Int
hmGitTreeLim = asks hmGitTreeLimit

hmGitModules :: MonadThrow m => HMGitT m (P.Path P.Abs P.File)
hmGitModules = asks hmGitModulesFile >>= fromMonad (Nothing :: Maybe Void)

hmGitLoadModules :: (MonadIO m, MonadThrow m) => HMGitT m Toml.Table
hmGitLoadModules = do
    mfile <- P.toFilePath <$> hmGitModules
    liftIO (T.readFile mfile)
        >>= fromMonad (Nothing :: Maybe Void) . Toml.parseTomlDoc mfile

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

