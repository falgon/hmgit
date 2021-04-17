module Main where


import           HMGit                           (HMGitConfig (..), runHMGit)
import           HMGit.Commands                  (Cmd (..))
import           HMGit.Commands.Plumbing.CatFile

import qualified Data.ByteString.UTF8            as B
import qualified Options.Applicative             as OA
import           System.FilePath                 ((</>))
import           System.IO                       (hPrint, stderr)

programOptions :: OA.Parser Cmd
programOptions = OA.hsubparser $ mconcat [
    catFileCmd
  ]

optsParser :: OA.ParserInfo Cmd
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "the subset of awesome content tracker"
  ]

main :: IO ()
main = do
    opts <- OA.customExecParser (OA.prefs OA.showHelpOnError) optsParser
    let hmGitConfig = HMGitConfig {
        hmGitDir = (</> ".hmgit")
      , hmGitTreeLimit = 1000
    }
    case opts of
        CmdCatFile mode object -> runHMGit (catFile (getCatFileRunner mode) (B.fromString object)) hmGitConfig
            >>= either (hPrint stderr) pure
