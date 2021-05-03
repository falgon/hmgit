{-# LANGUAGE ExplicitNamespaces, Rank2Types, TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module HMGit.Internal.Exceptions (
    invalidArgument
  , noSuchThing
  , BugException (..)
  , MonadThrowable (..)
) where

import           Control.Arrow                ((|||))
import           Control.Exception.Safe       (Exception, MonadThrow, throw,
                                               throwString)
import           Control.Monad                ((>=>))
import           Control.Monad.Error          (ErrorT, runErrorT)
import           Control.Monad.Trans.Except   (ExceptT, runExceptT)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.List     (ListT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Natural              (type (~>))
import           Data.Functor.Identity        (Identity (..))
import           GHC.IO.Exception             (IOErrorType (..),
                                               IOException (..))

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

noSuchThing :: String -> IOException
noSuchThing = ioEx NoSuchThing

newtype BugException = BugException String
    deriving Show

instance Exception BugException where

class MonadThrowable m where
    fromMonad :: (MonadThrow n, Exception e) => Maybe e -> m ~> n

instance Exception e => MonadThrowable (Either e) where
    fromMonad Nothing  = throw ||| pure
    fromMonad (Just e) = const (throw e) ||| pure

instance MonadThrowable Maybe where
    fromMonad Nothing  = throwString "Nothing" `maybe` pure
    fromMonad (Just e) = throw e `maybe` pure

instance MonadThrowable Identity where
    fromMonad _ = pure . runIdentity

instance MonadThrowable [] where
    fromMonad Nothing []  = throwString "empty"
    fromMonad (Just e) [] = throw e
    fromMonad _ (x:_)     = pure x

instance (Exception e, MonadThrowable m) => MonadThrowable (ExceptT e m) where
    fromMonad e = fromMonad e . runExceptT >=> fromMonad e

instance MonadThrowable m => MonadThrowable (MaybeT m) where
    fromMonad e = fromMonad e . runMaybeT >=> fromMonad e

instance MonadThrowable m => MonadThrowable (IdentityT m) where
    fromMonad e = fromMonad e . runIdentityT

instance MonadThrowable m => MonadThrowable (ListT m) where
    fromMonad e = fromMonad e . runListT >=> fromMonad e

instance (Exception e, MonadThrowable m) => MonadThrowable (ErrorT e m) where
    fromMonad e = fromMonad e . runErrorT >=> fromMonad e
