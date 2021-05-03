{-# LANGUAGE LambdaCase #-}
module Main where

import           HMGit                              (HMGitConfig (..), HMGitT,
                                                     getHMGitPath, runHMGit)
import           HMGit.Commands                     (Cmd (..))
import           HMGit.Commands.Plumbing.CatFile
import           HMGit.Commands.Plumbing.HashObject
import           HMGit.Commands.Plumbing.LsFiles

import           Control.Exception.Safe             (MonadThrow, tryAny)
import           Control.Monad.IO.Class             (MonadIO)
import qualified Data.ByteString.UTF8               as B
import           Data.Functor                       ((<&>))
import qualified Options.Applicative                as OA
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr)

programOptions :: (MonadThrow m, MonadIO m) => OA.Parser (Cmd m)
programOptions = OA.hsubparser $ mconcat [
    catFileCmd
  , hashObjectCmd
  , lsFilesCmd
  ]

optsParser :: (MonadThrow m, MonadIO m) => OA.ParserInfo (Cmd m)
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "the subset of awesome content tracker"
  ]

cmdToHMGitT :: (MonadThrow m, MonadIO m) => Cmd m -> HMGitT m ()
cmdToHMGitT (CmdCatFile mode object) = catFile mode (B.fromString object)
cmdToHMGitT (CmdHashObject objType mode fpath) = hashObject mode objType fpath
cmdToHMGitT (CmdLsFiles mode globs) = lsFiles mode globs

main :: IO ()
main = do
    cmd <- OA.customExecParser (OA.prefs OA.showHelpOnError) optsParser <&> cmdToHMGitT
    tryAny (getHMGitPath ".hmgit") >>= \case
        Left err -> hPutStrLn stderr (show err) *> exitFailure
        Right hmGitPath -> runHMGit cmd (HMGitConfig { hmGitDir = hmGitPath, hmGitTreeLimit = 1000 })
