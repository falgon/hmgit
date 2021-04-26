module Main where

import           HMGit                              (HMGitConfig (..), HMGitT,
                                                     getHMGitPath, runHMGit)
import           HMGit.Commands                     (Cmd (..))
import           HMGit.Commands.Plumbing.CatFile
import           HMGit.Commands.Plumbing.HashObject
import           HMGit.Commands.Plumbing.LsFiles

import           Control.Exception.Safe             (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO)
import qualified Data.ByteString.UTF8               as B
import           Data.Functor                       ((<&>))
import qualified Options.Applicative                as OA

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
cmdToHMGitT (CmdCatFile mode object) = catFile (getCatFileRunner mode) (B.fromString object)
cmdToHMGitT (CmdHashObject objType mode fpath) = hashObject (getHashObjectRunner mode) objType fpath
cmdToHMGitT (CmdLsFiles mode globs) = lsFiles (getLsFilesRunner mode) globs

main :: IO ()
main = do
    hmGitPath <- getHMGitPath ".hmgit"
    OA.customExecParser (OA.prefs OA.showHelpOnError) optsParser
        <&> cmdToHMGitT
        >>= flip runHMGit (HMGitConfig { hmGitDir = hmGitPath, hmGitTreeLimit = 1000 })
