import           HMGit.Internal.Parser.Pathspecs (removeDots)

import           Test.Hspec                      (parallel)
import           Test.Hspec.Contrib.HUnit        (fromHUnitTest)
import           Test.Hspec.Core.Runner          (Config (..), defaultConfig,
                                                  evaluateSummary, runSpec)
import           Test.HUnit

internal :: Test
internal = TestLabel "Internal" $ TestList [
    TestLabel "Parser" $ TestList [
        TestLabel "Pathspecs" $ TestList [
            TestLabel "removeDots" $ TestList [
                let t1 = "hoge/foo/bar" in t1 ~: removeDots t1 ~?= Just "hoge/foo/bar"
              , let t2 = "" in t2 ~: removeDots t2 ~?= Just ""
              , let t3 = "hoge/../bar" in t3 ~: removeDots t3 ~?= Just "bar"
              , let t4 = "hoge/foo/.." in t4 ~: removeDots t4 ~?= Just "hoge"
              , let t5 = "hoge/foo/../bar/../" in t5 ~: removeDots t5 ~?= Just "hoge"
              , let t6 = ".." in t6 ~: removeDots t6 ~?= Nothing
              , let t7 = "../" in t7 ~: removeDots t7 ~?= Nothing
              , let t8 = "home/hoge./../" in t8 ~: removeDots t8 ~?= Just "home"
              , let t9 = "home/.." in t9 ~: removeDots t9 ~?= Just ""
              , let t10 = "home/../.." in t10 ~: removeDots t10 ~?= Nothing
              , let t11 = "/home/.." in t11 ~: removeDots t11 ~?= Nothing
              ]
          ]
      ]
    ]

testMain :: Test
testMain = TestLabel "HMGit" $
    TestList [
        internal
    ]

main :: IO ()
main = runSpec (parallel $ fromHUnitTest testMain) (defaultConfig { configPrintCpuTime = True })
    >>= evaluateSummary
