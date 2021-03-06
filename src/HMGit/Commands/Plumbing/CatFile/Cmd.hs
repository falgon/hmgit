module HMGit.Commands.Plumbing.CatFile.Cmd (
    catFileCmd
) where


import           HMGit.Commands                       (Cmd (..))
import           HMGit.Commands.Plumbing.CatFile.Core (CatFile, catOptObjectPP,
                                                       catOptObjectSize,
                                                       catOptObjectType)

import           Control.Exception.Safe               (MonadThrow)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Foldable                        (asum)
import qualified Options.Applicative                  as OA

catFileMode :: (MonadThrow m, MonadIO m) => OA.Parser (CatFile m)
catFileMode = asum [
    OA.flag' catOptObjectType $ mconcat [
        OA.short 't'
      , OA.help "Instead of content, show the object type identified by <object>."
      ]
  , OA.flag' catOptObjectSize $ mconcat [
        OA.short 's'
      , OA.help "Instead of the content, show the object size identified by <object>."
      ]
  , OA.flag' catOptObjectPP $ mconcat [
        OA.short 'p'
      , OA.help "Pretty-print the contents of <object> based on its type."
      ]
  , OA.strArgument $ mconcat [
        OA.metavar "<type>"
      , OA.help $ unwords [
            "Typically this matches the real of <object> but asking for a"
          , "type that can trivially be dereferenced from the given <object>"
          , "is also permitted. An example is to ask for a \"tree\" with"
          , "<object> being a commit object that contains it, or to ask for"
          , "a \"blob\" with <object> being a tag object that points at it."
          ]
      ]
  ]

objectName :: OA.Parser String
objectName = OA.strArgument $ mconcat [
    OA.metavar "<object>"
  , OA.help "Set as an option dedicated to cat-file"
  ]

catFileCmd :: (MonadThrow m, MonadIO m) => OA.Mod OA.CommandFields (Cmd m)
catFileCmd = OA.command "cat-file"
    $ OA.info (CmdCatFile <$> (OA.helper <*> catFileMode) <*> objectName)
    $ OA.progDesc "Provide content or type and size information for repository objects"

