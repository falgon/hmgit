module HMGit.Commands.Porcelain.Init.Cmd (
    initCmd
) where

import           HMGit.Commands                     (Cmd (..))
import           HMGit.Commands.Porcelain.Init.Core (Init, initDefault,
                                                     initQuiet)

import           Control.Exception.Safe             (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO)
import           Data.Foldable                      (asum)
import qualified Options.Applicative                as OA

initMode :: (MonadThrow m, MonadIO m) => OA.Parser (String -> Init m)
initMode = asum [
    OA.flag' initQuiet $ mconcat [
        OA.short 'q'
      , OA.long "quiet"
      , OA.help "Only print error and warning messages; all other output will be suppressed."
      ]
  , pure initDefault
  ]

repositoryName :: OA.Parser String
repositoryName = OA.strArgument $ mconcat [
    OA.metavar "<directory>"
  , OA.value "."
  , OA.help ""
  ]

initCmd :: (MonadThrow m, MonadIO m) => OA.Mod OA.CommandFields (Cmd m)
initCmd = OA.command "init"
    $ OA.info (CmdInit <$> (OA.helper <*> initMode) <*> repositoryName)
    $ OA.progDesc "Create an empty Git repository or reinitialize an existing one"
