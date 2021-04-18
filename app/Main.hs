module Main where

import           HMGit                              (HMGitConfig (..), HMGitT,
                                                     runHMGit)
import           HMGit.Commands                     (Cmd (..))
import           HMGit.Commands.Plumbing.CatFile
import           HMGit.Commands.Plumbing.HashObject

import           Control.Exception.Safe             (MonadThrow)
import qualified Data.ByteString.UTF8               as B
import qualified Options.Applicative                as OA
import           System.FilePath                    ((</>))
import           System.IO                          (hPrint, stderr)

programOptions :: MonadThrow m => OA.Parser (Cmd m)
programOptions = OA.hsubparser $ mconcat [
    catFileCmd
  , hashObjectCmd
  ]

optsParser :: MonadThrow m => OA.ParserInfo (Cmd m)
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "the subset of awesome content tracker"
  ]

cmdToHMGitT :: MonadThrow m => Cmd m -> HMGitT IO (m ())
cmdToHMGitT (CmdCatFile mode object) = catFile (getCatFileRunner mode) (B.fromString object)
cmdToHMGitT (CmdHashObject objType mode fpath) = hashObject (getHashObjectRunner mode) objType fpath

hmGitConfig :: HMGitConfig
hmGitConfig = HMGitConfig {
    hmGitDir = (</> ".hmgit")
  , hmGitTreeLimit = 1000
  }

main :: IO ()
main = OA.customExecParser (OA.prefs OA.showHelpOnError) optsParser
    >>= flip runHMGit hmGitConfig . cmdToHMGitT
    >>= either (hPrint stderr) pure
