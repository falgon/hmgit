module HMGit.Commands.Porcelain.Add.Cmd (
    addCmd
) where

import           HMGit.Commands                    (Cmd (..))
import           HMGit.Commands.Porcelain.Add.Core (Add (..), addDefault,
                                                    addDryRun)

import           Control.Exception.Safe            (MonadCatch)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Foldable                     (asum)
import qualified Options.Applicative               as OA

addMode :: (MonadCatch m, MonadIO m) => OA.Parser (Add m)
addMode = asum [
    OA.flag' addDryRun $ mconcat [
        OA.long "dry-run"
      , OA.short 'n'
      , OA.help "Don’t actually add the file(s), just show if they exist and/or will be ignored."
      ]
    , pure addDefault
    ]

addPathspecs :: OA.Parser [String]
addPathspecs = OA.many $ OA.argument OA.str $ mconcat [
    OA.metavar "<pathspec>..."
  , OA.help $ unwords [
        "Files to add content from."
      , "see the pathspec entry in gitglossary(7)."
      ]
  ]

addCmd :: (MonadCatch m, MonadIO m) => OA.Mod OA.CommandFields (Cmd m)
addCmd = OA.command "add"
    $ OA.info (CmdAdd <$> (OA.helper <*> addMode) <*> addPathspecs)
    $ OA.progDesc "Add file contents to the index"
