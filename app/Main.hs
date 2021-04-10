module Main where

import           HMGit.Commands.CatFile (CatOpt (..), catFile, catOptObject,
                                         catOptObjectPP, catOptObjectSize,
                                         catOptObjectType)

import qualified Data.ByteString.UTF8   as B
import           Data.Foldable          (asum)
import           Data.String            (IsString (..))
import qualified Options.Applicative    as OA
import           System.IO              (hPrint, stderr)

data Cmd = CmdCatFile

newtype CatFileMode = CatFileMode { getRunner :: CatOpt }

instance IsString CatFileMode where
    fromString = CatFileMode . catOptObject . read

data Opts = Opts
    { optCmd         :: Cmd
    , optCatFileMode :: CatFileMode
    , optObject      :: String
    }

catFileCmd :: OA.Mod OA.CommandFields Cmd
catFileCmd = OA.command "cat-file"
    $ OA.info (pure CmdCatFile)
    $ OA.progDesc "Provide content or type and size information for repository objects"

catFileMode :: OA.Parser CatFileMode
catFileMode = asum [
    OA.flag' (CatFileMode catOptObjectType) $ mconcat [
        OA.short 't'
      , OA.help "Instead of content, show the object type identified by <object>."
      ]
  , OA.flag' (CatFileMode catOptObjectSize) $ mconcat [
        OA.short 's'
      , OA.help "Instead of the content, show the object size identified by <object>."
      ]
  , OA.flag' (CatFileMode catOptObjectPP) $ mconcat [
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
objectName = OA.strArgument $ OA.metavar "<object>"

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> OA.hsubparser (mconcat [
        catFileCmd
      ])
    <*> catFileMode
    <*> objectName

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "the subset of awesome content tracker"
  ]

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    case optCmd opts of
        CmdCatFile -> catFile (getRunner (optCatFileMode opts)) (B.fromString $ optObject opts)
            >>= either (hPrint stderr) pure
