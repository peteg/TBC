{- Test By Convention: The TestSuite type and friends.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.TestSuite
    ( TestSuite(..) -- FIXME abstract
    , empty
    , Test(..) -- FIXME abstract
    , Result(..) -- FIXME abstract
    , TestFile

    , Config(..) -- FIXME abstract

    , runTestSuite
    , renderTAP
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM )

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | Global configuration.
data Config
    = MkConfig
      { hc :: String
      , hc_opts :: FilePath -> [String] -- ^ compiler flags as a function of the 'TestFile'.
      }

-- | A single test.
data Test
    = HUnit { tName :: String, tAssertion :: String }
    | QuickCheck { tName :: String, tAssertion :: String }
    | Sanity { tName :: String }
      deriving (Show)

-- | The result of a single 'Test'.
data Result
    = TestResultNone -- ^ FIXME Haven't run it yet.
    | TestResultSkip
    | TestResultToDo
    | TestResultSuccess
    | TestResultFailure
    | TestResultQuickCheckCounterExample
      deriving (Show)

-- | A hierarchical collection of tests.
data TestSuite
    = TestSuiteEmpty -- ^ The empty 'TestSuite'
    | TestSuiteError { tsError :: String } -- ^ An error occurred at this node.
    | TestSuiteGroup { tsFile :: FilePath, tsTests :: [(Test, Result)] } -- ^ A group of tests.
    | TestSuiteNode [TestSuite] -- ^ Hierarchy.
      deriving (Show)

empty :: TestSuite
empty = TestSuiteEmpty

type TestFile = FilePath

-------------------------------------------------------------------

-- | FIXME this is a bit functorial, a bit monadic.
runTestSuite :: Config -> TestSuite -> IO TestSuite
runTestSuite config ts =
    case ts of
      TestSuiteEmpty {} -> return ts
      TestSuiteError {} -> return ts

      TestSuiteGroup {} ->
        do trs <- runTests config (tsFile ts) (tsTests ts)
           return $ ts{tsTests = trs}

      TestSuiteNode ts' -> TestSuiteNode `liftM` mapM (runTestSuite config) ts'

runTests :: Config -> FilePath -> [(Test, Result)] -> IO [(Test, Result)]
runTests config f ts =
  do putStrLn $ "system $ " ++ hc config ++ " " ++ concat [ ' ' : a | a <- hc_opts config f ]
     putStrLn ghci_stdin
     error "runTests"
  where
     ghci_stdin = (foldr testCommand id ts) ""

-- | What we need to ask GHCi in order to run this test.
-- FIXME skip tests that already have results.
testCommand :: (Test, Result) -> ShowS -> ShowS
testCommand (t, TestResultNone) acc =
    case t of
      HUnit {} -> acc . hunit (tName t) (tAssertion t)
      QuickCheck {} -> acc . quickcheck (tName t) (tAssertion t)
testCommand _ acc = acc

hunit :: String -> String -> ShowS
hunit name assertion =
    showString "-- >>" . showString name . showString "<<\n"
  . showString "runTestTT $ TestCase " . showString assertion . showString "\n"

quickcheck :: String -> String -> ShowS
quickcheck name assertion =
    showString "-- >>" . showString name . showString "<<\n"
  . showString "test " . showString assertion . showString "\n"

-------------------------------------------------------------------

renderTAP :: TestSuite -> IO String
renderTAP = return . show
