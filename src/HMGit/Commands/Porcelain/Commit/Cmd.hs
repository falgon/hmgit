module HMGit.Commands.Porcelain.Commit.Cmd (
    commitCmd
) where

import           HMGit.Commands                       (Cmd (..))
import           HMGit.Commands.Porcelain.Commit.Core (Commit (..),
                                                       CommitCfg (..),
                                                       commitDefault)

import           Control.Exception.Safe               (MonadCatch)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Foldable                        (asum)
import qualified Options.Applicative                  as OA

commitMode :: (MonadIO m, MonadCatch m) => OA.Parser (Commit m)
commitMode = asum [
    pure commitDefault
  ]

optCommitMessage :: OA.Parser String
optCommitMessage = OA.option OA.str $ mconcat [
    OA.metavar "<msg>"
  , OA.short 'm'
  , OA.long "message"
  , OA.help "Get the name, ignoring whether it is a real name or a candidate name"
  ]

optCommitAuthor :: OA.Parser String
optCommitAuthor = OA.option OA.str $ mconcat [
    OA.metavar "<author>"
  , OA.value mempty
  , OA.long "author"
  , OA.help $ unwords [
        "Set the commit author."
      , "Specify an explicit author using"
      , "the standard A U Thor <author@example.com> format."
      , "If nothing is specified, HMGit looks for the"
      , "HMGIT_AUTHOR_NAME and HMGIT_AUTHOR_EMAIL environment variables."
      ]
  ]

-- | The commit command field
commitCmd :: (MonadIO m, MonadCatch m) => OA.Mod OA.CommandFields (Cmd m)
commitCmd = OA.command "commit"
    $ OA.info (CmdCommit
        <$> (OA.helper <*> commitMode)
        <*> (CommitCfg
            <$> optCommitMessage
            <*> optCommitAuthor))
    $ OA.progDesc "Record changes to the repository"
