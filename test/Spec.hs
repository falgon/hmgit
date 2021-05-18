{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Test.Utils                      (relativeProjRoot)

import           HMGit                           (HMGitConfig (..), HMGitT,
                                                  runHMGit)
import           HMGit.Internal.Parser.Pathspecs (lsMatches, pathspecs)

import           Control.Applicative             (Alternative)
import           Control.Exception.Safe          (MonadMask, bracket, tryAny)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans             (lift)
import           Data.Either                     (isLeft)
import           Data.Functor                    (($>))
import           Path                            (Abs, Dir, File, Rel)
import qualified Path                            as P
import qualified Path.IO                         as P
import           Test.Hspec                      (parallel)
import           Test.Hspec.Contrib.HUnit        (fromHUnitTest)
import           Test.Hspec.Core.Runner          (Config (..), defaultConfig,
                                                  evaluateSummary, runSpec)
import           Test.HUnit

testPathspecs :: (MonadIO m, MonadMask m, Alternative m) => HMGitT m Test
testPathspecs = do
    t1 <- pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["."]
    t2 <- pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["*.txt"]
    t3 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cat.hs")) ["*.hs"]
    t4 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cat.hs")) ["*c[ao]t.?s"]
    t5 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")) ["*c[ao]t.?s"]
    t6 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")) ["foo"]
    t7 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")) ["foo/"]
    t8 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")) ["foo/bar"]
    t9 <- pathspecs' (P.Rel $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")) ["foo/bar/"]
    t10 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent)
      $ const
      $ pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["../"]
    t11 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent)
      $ const
      $ pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["../foo/.."]
    t12 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo/bar")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent . P.parent)
      $ const
      $ tryAny
      $ pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["*.txt"]
    t13 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo/bar")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent . P.parent)
      $ const
      $ pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) ["../../*.txt"]
    t14 <- pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) []
    t15 <- pathspecs' (P.Rel $(P.mkRelFile "hello.txt")) [
        P.toFilePath (P.parent hmGitPath) <> "*"
      ]
    pure $ TestLabel "pathspecs" $ TestList [
        "hello.txt: ." ~: t1 ~?= "hello.txt"
      , "hello.txt: *.txt" ~: t2 ~?= "hello.txt"
      , "foo/bar/hoge/piyo/cat.hs: *.hs" ~: t3 ~?= "foo/bar/hoge/piyo/cat.hs"
      , "foo/bar/hoge/piyo/cat.hs: *c[ao]t.?s" ~: t4 ~?= "foo/bar/hoge/piyo/cat.hs"
      , "foo/bar/hoge/piyo/cot.hs: *c[ao]t.?s" ~: t5 ~?= "foo/bar/hoge/piyo/cot.hs"
      , "foo/bar/hoge/piyo/cot.hs: foo" ~: t6 ~?= "foo/bar/hoge/piyo/cot.hs"
      , "foo/bar/hoge/piyo/cot.hs: foo/" ~: t7 ~?=  "foo/bar/hoge/piyo/cot.hs"
      , "foo/bar/hoge/piyo/cot.hs: foo/bar" ~: t8 ~?= "foo/bar/hoge/piyo/cot.hs"
      , "foo/bar/hoge/piyo/cot.hs: foo/bar/" ~: t9 ~?= "foo/bar/hoge/piyo/cot.hs"
      , "hello.txt: ../" ~: t10 ~?= "../hello.txt"
      , "hello.txt: ../foo/.." ~: t11 ~?= "../hello.txt"
      , "hello.txt: *.txt" ~: isLeft t12 ~?= True
      , "hello.txt: ../../*.txt" ~: t13 ~?= "../../hello.txt"
      , "hello.txt: " ~: t14 ~?= "hello.txt"
      , "hello.txt: " <> P.toFilePath (P.parent hmGitPath) <> "*" ~: t15 ~?= "hello.txt"
      ]
    where
        pathspecs' x y = P.getCurrentDir
            >>= flip (`pathspecs` x) y

testLsMatches :: (MonadIO m, MonadMask m) => HMGitT m Test
testLsMatches = do
    t1 <- lsMatches' ["*.txt"]
    t2 <- lsMatches' ["*.hs"]
    t3 <- lsMatches' ["*.txt", "*.hs"]
    t4 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo/bar/hoge")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent . P.parent . P.parent)
        $ const
        $ lsMatches' ["*.hs"]
    t5 <- lsMatches' []
    t6 <- bracket
        (lift ((P.</>) <$> P.getCurrentDir <*> pure $(P.mkRelDir "foo/bar/hoge")
            >>= \x -> P.setCurrentDir x $> x))
        (lift . P.setCurrentDir . P.parent . P.parent . P.parent)
        $ const
        $ lsMatches' ["../../../*.txt"]
    t7 <- lsMatches' ["hello.txt"]
    t8 <- lsMatches' [P.toFilePath $ P.parent hmGitPath P.</> $(P.mkRelFile "hello.txt")]
    pure $ TestLabel "lsMatches" $ TestList [
        "*.txt" ~: t1 ~?= [ $(P.mkRelFile "hello.txt") ]
      , "*.hs" ~: t2 ~?= [
            $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat1.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat.hs")
          , $(P.mkRelFile "foo/bar/var/hello.hs")
          ]
      , "*.txt *.hs" ~: t3 ~?= [
            $(P.mkRelFile "hello.txt")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat1.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat.hs")
          , $(P.mkRelFile "foo/bar/var/hello.hs")
          ]
      , "*.hs" ~: t4 ~?= [
            $(P.mkRelFile "foo/bar/hoge/piyo/cot.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat1.hs")
          , $(P.mkRelFile "foo/bar/hoge/piyo/cat.hs")
          ]
      , "<no pathspecs>" ~: t5 ~?= mempty
      , "../../../*.txt" ~: t6 ~?= [ $(P.mkRelFile "hello.txt") ]
      , "hello.txt" ~: t7 ~?= [ $(P.mkRelFile "hello.txt") ]
      , P.toFilePath (P.parent hmGitPath P.</> $(P.mkRelFile "hello.txt")) ~: t8 ~?= [
            $(P.mkRelFile "hello.txt")
          ]
      ]
    where
        lsMatches' xs = P.getCurrentDir
            >>= flip lsMatches xs

testMain :: (MonadIO m, MonadMask m, Alternative m) => HMGitT m Test
testMain = do
    testPathspecs' <- testPathspecs
    testLsMatches' <- testLsMatches
    pure $
        TestLabel "HMGit" $ TestList [
            TestLabel "Internal" $ TestList [
                TestLabel "Parser" $ TestList [
                    TestLabel "Pathspecs" $ TestList [
                        testPathspecs'
                      , testLsMatches'
                      ]
                  ]
              ]
          ]

hmGitPath :: P.Path P.Abs P.Dir
hmGitPath = $(P.mkAbsDir $(relativeProjRoot "test/external/hmgit-test-repo/hmgit-db"))

main :: IO ()
main = do
    P.setCurrentDir $ P.parent hmGitPath
    tests <- runHMGit testMain $ HMGitConfig {
        hmGitDir = hmGitPath
      , hmGitTreeLimit = 1000
      }
    runSpec (parallel $ fromHUnitTest tests) (defaultConfig { configPrintCpuTime = True })
        >>= evaluateSummary
