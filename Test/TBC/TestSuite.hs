{- Test By Convention: The TestSuite type and friends.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.TestSuite
    ( TestSuite(..) -- FIXME abstract
    , TestGroup(..) -- FIXME
    , empty
    , Test(..) -- FIXME abstract
    , Result(..) -- FIXME abstract
    , TestFile
    , Convention

    , runTestSuite
    , renderTAP

    , conventionalIterator
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM, when )

import Data.Maybe ( catMaybes )
import Data.List ( isInfixOf, nub )

import System.IO -- ( hClose, hFlush, hGetContents, hPutStr )

import Test.TBC.Drivers ( Driver(hci_send_cmd) )
import Test.TBC.FoldDir ( Iterator, ItResult(..) )

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | A /convention/ maps a line in a 'TestFile' into a function that
-- runs the test.
type Convention = FilePath -> String -> Maybe Test

-- | A single test.
data Test
    = Test
      { tName :: String
      , tRun :: Driver -> IO Result
      }

instance Eq Test where
    t == t' = tName t == tName t'

data TestGroup
    = MkTestGroup
      { tgFile :: FilePath
      , tgTests :: [Test]
      }

-- | A hierarchical collection of tests.
data TestSuite
    = TestSuiteEmpty -- ^ The empty 'TestSuite'
    | TestSuiteError { tsError :: String } -- ^ An error occurred at this node. FIXME TOSS
    | TestSuiteGroup TestGroup
    | TestSuiteNode [TestSuite] -- ^ Hierarchy.

-- | The result of a single 'Test'.
data Result
    = TestResultSkip
    | TestResultToDo
    | TestResultSuccess
    | TestResultFailure
    | TestResultQuickCheckCounterExample
      deriving (Show)

data TestGroupResult
    = MkTestGroupResult
      { tgrFile :: FilePath
      , tgrTestsResults :: [(Test, Result)]
      }

empty :: TestSuite
empty = TestSuiteEmpty

type TestFile = FilePath

-------------------------------------------------------------------

-- | FIXME this is a bit functorial, a bit monadic.
runTestSuite :: Driver -> TestSuite -> IO ()
runTestSuite driver ts0 =
  -- FIXME bracket, exceptions
  do rTS ts0
--     hc_on_exit config
  where
    load_file f = ":l " ++ f ++ "\n"

    rTS ts =
      case ts of
        TestSuiteEmpty {} -> return ()
        TestSuiteError {} -> return ()

        TestSuiteGroup tg ->
          do cout <- hci_send_cmd driver (load_file (tgFile tg))
             when (not ("Ok, modules loaded" `isInfixOf` last cout)) $
                  error "Compilation problems."

             trs <- mapM (runTest driver) (tgTests tg)
             return ()
--              return $ ts{tsTests = trs}

        TestSuiteNode ts' -> mapM_ rTS ts'

-- | Run the 'Test's in a 'TestFile'.
-- FIXME GHCi strings hardwired.
-- FIXME need the TestFile any more?
runTest :: Driver -> Test -> IO (Test, Result)
runTest driver t =
  do r <- tRun t driver
     return (t, r)

-------------------------------------------------------------------

renderTAP :: TestSuite -> IO String
renderTAP = error "renderTAP"

-------------------------------------------------------------------



-- | Apply a list of conventions to the guts of a 'TestFile'.
applyConventions :: [Convention] -> FilePath -> String -> [Test]
applyConventions cs f = catMaybes . applyCs . lines
    where applyCs ls = [ c f l | l <- ls, c <- cs ]

-- FIXME hierarchy needs some help from foldDir
conventionalIterator :: [Convention] -> Iterator TestSuite
conventionalIterator cs suite f =
    do putStrLn $ "conventionIterator: " ++ f
       ts <- applyConventions cs f `liftM` readFile f
       let suite' = TestSuiteGroup (MkTestGroup { tgFile = f
                                                , tgTests = nub ts })
       return (Continue, TestSuiteNode [suite, suite'])




{-
This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}
