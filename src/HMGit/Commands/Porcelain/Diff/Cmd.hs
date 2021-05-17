module HMGit.Commands.Porcelain.Diff.Cmd (
    diffCmd
) where

import           HMGit.Commands                     (Cmd (..))
import           HMGit.Commands.Porcelain.Diff.Core (Diff (..), diffShow)

import           Control.Exception.Safe             (MonadCatch)
import           Control.Monad                      (MonadPlus)
import           Control.Monad.IO.Class             (MonadIO)
import           Data.Foldable                      (asum)
import qualified Options.Applicative                as OA

diffMode :: (MonadCatch m, MonadIO m, MonadPlus m) => OA.Parser (Diff m)
diffMode = asum [
    OA.flag' (Diff $ const $ pure ()) $ mconcat [
        OA.long "quiet"
      , OA.help "Disable all output of the program."
      ]
  , pure diffShow
  ]

diffPath :: OA.Parser [FilePath]
diffPath = OA.many $ OA.argument OA.str $ mconcat [
    OA.metavar "<path>..."
  , OA.help $ unwords [
        "The <paths> parameters, when given,"
       , "are used to limit the diff to the named paths"
       , "(you can give directory names and get diff for all files under them)."
       ]
  ]

diffCmd :: (MonadCatch m, MonadIO m, MonadPlus m) => OA.Mod OA.CommandFields (Cmd m)
diffCmd = OA.command "diff"
    $ OA.info (CmdDiff <$> (OA.helper <*> diffMode) <*> diffPath)
    $ OA.progDesc "Show changes between commits, commit and working tree, etc"
