module HMGit.Commands.Plumbing.HashObject.Cmd (
    hashObjectCmd
) where

import           HMGit.Commands                          (Cmd (..))
import           HMGit.Commands.Plumbing.HashObject.Core (HashObject,
                                                          hashObjectShow,
                                                          hashObjectWrite)
import           HMGit.Internal.Parser                   (ObjectType (..))

import           Control.Exception.Safe                  (MonadThrow)
import           Control.Monad.IO.Class                  (MonadIO)
import           Data.Foldable                           (asum)
import qualified Options.Applicative                     as OA

hashObjectMode :: (MonadThrow m, MonadIO m) => OA.Parser (HashObject m)
hashObjectMode = asum [
    OA.flag' hashObjectWrite $ mconcat [
        OA.short 'w'
      , OA.help "Actually write the object into the object database."
      ]
  , pure hashObjectShow
  ]

hashObjectType :: OA.Parser ObjectType
hashObjectType = OA.option OA.auto $ mconcat [
    OA.short 't'
  , OA.help "Specify the type"
  , OA.metavar "<type>"
  , OA.value Blob
  ]

hashObjectFilePath :: OA.Parser String
hashObjectFilePath = asum [
    OA.flag' "/dev/stdin" $ mconcat [
        OA.long "stdin"
      , OA.help "Read the object from standard input instead of from a file."
      ]
  , OA.strArgument $ mconcat [
        OA.metavar "<file>"
      , OA.help "File path"
      ]
  ]

hashObjectCmd :: (MonadThrow m, MonadIO m) => OA.Mod OA.CommandFields (Cmd m)
hashObjectCmd = OA.command "hash-object"
    $ OA.info (CmdHashObject <$> (OA.helper <*> hashObjectType) <*> hashObjectMode <*> hashObjectFilePath)
    $ OA.progDesc "Compute object ID and optionally creates a blob from a file"
