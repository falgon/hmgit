{-# LANGUAGE ExplicitForAll, FlexibleContexts, TupleSections #-}
module HMGit.Internal.Utils (
    stateEmpty
  , strictOne
  , foldMapM
  , foldChoice
  , foldChoiceM
  , first3M
  , makeRelativeEx
  , (?*>)
  , (??)
  , bothM
  , hexStr
) where

import           HMGit.Internal.Exceptions (MonadThrowable (..),
                                            invalidArgument)


import           Control.Applicative       (Alternative (..))
import           Control.Exception.Safe    (Exception, MonadThrow,
                                            StringException (..), throw)
import           Control.Monad             (MonadPlus (..), (>=>))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Foldable             (foldlM)
import qualified Data.List.NonEmpty        as LN
import           Data.Monoid               (Alt (..))
import           Data.MonoTraversable      (Element, MonoFoldable, oconcatMap)
import           Data.Tuple.Extra          (first)
import           GHC.Stack                 (callStack)
import           System.Directory          (canonicalizePath)
import           System.FilePath           (isRelative, makeRelative,
                                            takeDirectory, takeDrive, (</>))
import           Text.Printf               (PrintfArg, printf)

stateEmpty :: (Foldable t, MonadPlus m) => (a, t b) -> m a
stateEmpty x
    | not $ null $ snd x = mzero
    | otherwise = pure $ fst x

nonEmpty :: MonadThrow m => [a] -> m (LN.NonEmpty a)
nonEmpty = fromMonad (Just $ StringException "a given list is empty" callStack)
    . LN.nonEmpty

strictOne :: (MonadPlus m, MonadThrow m) => [a] -> m a
strictOne = nonEmpty >=> stateEmpty . first head . LN.splitAt 1

foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
foldMapM f = foldlM (\acc a -> do { w <- f a; return $! mappend acc w }) mempty

foldChoice :: (Foldable t, Alternative f)
    => (a -> f b)
    -> t a
    -> f b
foldChoice f = getAlt . foldMap (Alt . f)

foldChoiceM :: (Monad m, Alternative f, Foldable t)
    => (a -> m (f b))
    -> t a
    -> m (f b)
foldChoiceM f = fmap getAlt . foldMapM (fmap Alt . f)

{-# INLINE hexStr #-}
hexStr :: (MonoFoldable mono, PrintfArg (Element mono))
    => mono
    -> String
hexStr = oconcatMap (printf "%02x")

first3M :: Functor m => (a -> m a') -> (a, b, c) -> m (a', b, c)
first3M f (x,y,z) = (,y,z) <$> f x

newtype DifferentDrives = DifferentDrives String
    deriving Show

instance Exception DifferentDrives where

-- Inspired by Development.Shake.FilePath.makeRelativeEx
makeRelativeEx :: (MonadIO m, MonadThrow m)
    => FilePath
    -> FilePath
    -> m FilePath
makeRelativeEx pathA pathB
    | isRelative makeRelativePathAPathB = pure makeRelativePathAPathB
    | otherwise = do
        a' <- liftIO $ canonicalizePath pathA
        b' <- liftIO $ canonicalizePath pathB
        if takeDrive a' /= takeDrive b'
            then throw $ DifferentDrives "Two paths do not exist on the same drive"
            else makeRelativeEx' a' b'
    where
        makeRelativePathAPathB = makeRelative pathA pathB

        makeRelativeEx' a b = do
            let rel = makeRelative a b
                parent = takeDirectory a
            if isRelative rel
                then pure rel
                else if a /= parent
                    then (".." </>) <$> makeRelativeEx' parent b
                    -- Impossible: makeRelative should have succeeded in finding
                    -- a relative path once `a == "/"`.
                    else throw
                        $ invalidArgument
                        $ concat [
                            "Error calculating relative path from \""
                           , pathA
                           , "\" to \""
                           , show pathB
                           , "\""
                           ]

(?*>) :: Alternative f => Bool -> f a -> f a
b ?*> f = if b then f else empty

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = do
    x' <- f x
    (x',) <$> f y
