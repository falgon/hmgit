{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HMGit.Commands.Porcelain.Commit.Core (
    Commit (..)
  , CommitCfg (..)
  , commitDefault
) where

import           HMGit.Internal.Core            (storeObject, storeTree)
import qualified HMGit.Internal.Core            as OBJ (ObjectType (..))
import           HMGit.Internal.Core.Runner     (HMGitT)
import           HMGit.Internal.Core.Runner.API (hmGitBRName', hmGitBRPath',
                                                 hmGitLoadMasterHash)
import           HMGit.Internal.Utils           (hexStr)

import           Control.Exception.Safe         (MonadCatch)
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Data.ByteString.Lazy.Char8     as BLC
import qualified Data.ByteString.Lazy.UTF8      as BLU
import           Data.String                    (IsString (..))
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import           Data.Time.LocalTime            (TimeZone (..),
                                                 getCurrentTimeZone)
import qualified Path                           as P
import           System.Environment             (getEnv)
import           Text.Printf                    (printf)

data CommitCfg = CommitCfg {
    commitMessage :: String
  , commitAuthor  :: String
  }
  deriving Show

newtype Commit m = Commit { commit :: CommitCfg -> HMGitT m () }

authorTime :: forall m. (MonadIO m) => m String
authorTime = do
    timestamp <- round <$> liftIO getPOSIXTime :: m Integer
    utcOffsetSec <- (*60) . timeZoneMinutes <$> liftIO getCurrentTimeZone
    pure $ printf "%d %c%02d%02d"
        timestamp
        (if utcOffsetSec > 0 then '+' else '-')
        (abs utcOffsetSec `div` 3600)
        ((abs utcOffsetSec `div` 60) `mod` 60)

commitDefault :: (MonadIO m, MonadCatch m) => Commit m
commitDefault = Commit $ \ccfg -> do
    sha1 <- sequence [
        fromString . ("tree " <>) . hexStr <$> storeTree
      , maybe mempty (BLC.pack . printf "\nparent %s") <$> hmGitLoadMasterHash
      , (.) BLC.pack . printf "\nauthor %s %s" <$> cAuthor ccfg <*> authorTime
      , (.) BLC.pack . printf "\ncommitter %s %s\n\n" <$> cAuthor ccfg <*> authorTime
      , pure $ BLU.fromString $ commitMessage ccfg
      , pure "\n"
      ]
        >>= storeObject OBJ.Commit . mconcat
    hmGitBRPath'
        >>= liftIO . flip writeFile (hexStr sha1 <> "\n") . P.toFilePath
        >> printf "[%s (commit) %.7s] %s"
            <$> hmGitBRName'
            <*> pure (hexStr sha1)
            <*> pure (commitMessage ccfg)
        >>= liftIO . putStrLn
    where
        cAuthor ccfg
            | null (commitAuthor ccfg) =
                liftIO (printf "%s <%s>" <$> getEnv "HMGIT_AUTHOR_NAME" <*> getEnv "HMGIT_AUTHOR_EMAIL")
            | otherwise = pure $ commitAuthor ccfg
