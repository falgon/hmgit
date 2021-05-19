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
    OA.flag' (Diff $ const $ const $ pure ()) $ mconcat [
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

noPrefix :: OA.Parser Bool
noPrefix = OA.switch $ mconcat [
    OA.long "no-prefix"
  , OA.help "Do not show any source or destination prefix."
  ]

srcPrefix :: OA.Parser String
srcPrefix = OA.option OA.str $ mconcat [
    OA.long "src-prefix"
  , OA.value "a/"
  , OA.metavar "<prefix>"
  , OA.help "Show the given source prefix instead of \"a/\"."
  ]

dstPrefix :: OA.Parser String
dstPrefix = OA.option OA.str $ mconcat [
    OA.long "dst-prefix"
  , OA.value "b/"
  , OA.metavar "<prefix>"
  , OA.help "Show the given destination prefix instead of \"b/\"."
  ]

diffCmd :: (MonadCatch m, MonadIO m, MonadPlus m) => OA.Mod OA.CommandFields (Cmd m)
diffCmd = OA.command "diff"
    $ OA.info (CmdDiff <$> (OA.helper <*> diffMode) <*> diffPath <*> noPrefix <*> srcPrefix <*> dstPrefix)
    $ OA.progDesc "Show changes between commits, commit and working tree, etc"
