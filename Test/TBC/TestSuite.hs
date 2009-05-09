{- Test By Convention: The TestSuite type and friends.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.TestSuite
    ( TestSuite
    , TestGroup
    , Test
    , TestFile

    , conventionalTestSuiteIterator
    , runTestSuite
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Test.TBC.FoldDir ( Iterator )

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | A single test.
data Test
    = HUnit
    | QuickCheck
    | Sanity
      deriving (Show)

-- | The result of a single 'Test'.
data TestResult
    = TestResultSkip
    | TestResultToDo
    | TestResultSuccess
    | TestResultFailure
    | TestResultQuickCheckCounterExample
      deriving (Show)

-- | A flat collection of tests.
newtype TestGroup = MkTG [(Test, Maybe TestResult)]

-- | A hierarchical collection of tests. Nodes are labelled by 'TestGroup's.
data TestSuite
    = TestSuiteEmpty -- ^ The empty 'TestSuite'
    | TestSuiteError -- ^ An error occurred at this node.
    | TestSuiteGroup TestGroup -- ^ Lift a 'TestGroup' into a 'TestSuite'.
    | TestSuiteNode [TestSuite] -- ^ Hierarchy.

type TestFile = FilePath

-------------------------------------------------------------------

conventionalTestSuiteIterator :: Iterator TestSuite
conventionalTestSuiteIterator = undefined

-------------------------------------------------------------------

runTestSuite :: TestSuite -> IO TestSuite
runTestSuite = error "runTestSuite"

-------------------------------------------------------------------

renderTAP :: TestSuite -> IO String
renderTAP = error "renderTAP"
