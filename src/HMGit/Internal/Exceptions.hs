module HMGit.Internal.Exceptions (
    invalidArgument
  , nosuchThing
) where

import           GHC.IO.Exception (IOErrorType (..), IOException (..))

ioEx :: IOErrorType -> String -> IOException
ioEx errorType description = IOError {
    ioe_handle = Nothing
  , ioe_type = errorType
  , ioe_location = mempty
  , ioe_description = description
  , ioe_errno = Nothing
  , ioe_filename = Nothing
  }

invalidArgument :: String -> IOException
invalidArgument = ioEx InvalidArgument

nosuchThing :: String -> IOException
nosuchThing = ioEx NoSuchThing
