module HMGit.Commands.Porcelain.Status.Cmd (
    statusCmd
) where

import           HMGit.Commands                       (Cmd (..))
import           HMGit.Commands.Porcelain.Status.Core (Status, statusDefault,
                                                       statusShort)

import           Control.Exception.Safe               (MonadCatch)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Foldable                        (asum)
import qualified Options.Applicative                  as OA

statusMode :: (MonadCatch m, MonadIO m, OA.Alternative m) => OA.Parser (Status m)
statusMode = asum [
    OA.flag' statusShort $ mconcat [
        OA.short 's'
      , OA.long "short"
      , OA.help "Give the output in the short-format."
      ]
  , OA.flag' statusDefault $ mconcat [
        OA.long "long"
      , OA.help "Give the output in the long-format. This is the default."
      ]
  , pure statusDefault
  ]

pathspecs :: OA.Parser [FilePath]
pathspecs = OA.many $ OA.argument OA.str $ mconcat [
    OA.metavar "<pathspec>..."
  , OA.help "See the pathspec entry in gitglossary(7)."
  ]

statusCmd :: (MonadCatch m, MonadIO m, OA.Alternative m) => OA.Mod OA.CommandFields (Cmd m)
statusCmd = OA.command "status"
    $ OA.info (CmdStatus <$> (OA.helper <*> statusMode) <*> pathspecs)
    $ OA.progDesc "Show the working tree status"

