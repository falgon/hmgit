module HMGit.Commands.Plumbing.LsFiles.Cmd (
    lsFilesCmd
) where

import           HMGit.Commands                       (Cmd (..))
import           HMGit.Commands.Plumbing.LsFiles.Core (LsFiles, lsFilesDetail,
                                                       lsFilesShow)

import           Control.Exception.Safe               (MonadThrow)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Foldable                        (asum)
import qualified Options.Applicative                  as OA

lsFilesMode :: (MonadThrow m, MonadIO m) => OA.Parser (LsFiles m)
lsFilesMode = asum [
    OA.flag' lsFilesDetail $ mconcat [
        OA.short 's'
      , OA.long "stage"
      , OA.help "Show staged contents' mode bits, object name and stage number in the output."
      ]
   , pure lsFilesShow
   ]

fileNames :: OA.Parser [FilePath]
fileNames = OA.many $ OA.argument OA.str $ mconcat [
    OA.metavar "<files>..."
  , OA.help "Files to show. If no files are given all files which match the other specified criteria are shown."
  ]

lsFilesCmd :: (MonadThrow m, MonadIO m) => OA.Mod OA.CommandFields (Cmd m)
lsFilesCmd = OA.command "ls-files"
    $ OA.info (CmdLsFiles <$> (OA.helper <*> lsFilesMode) <*> fileNames)
    $ OA.progDesc "Show information about files in the index and the working tree"
