{- Test By Convention: the conventions themselves.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -
 - FIXME First cut
 -
 - Idea is to apply each of these tests to each line of a 'TestFile'
 - and collate the resulting 'TestSuite'.
 -
 - FIXME Import qualified.
 - FIXME tests that appear in block comments {- -} are still picked up.
 -}
module Test.TBC.Convention
    ( booltest
    , exception
    , hunit
    , quickcheck
--     , convention_mainPlannedTestSuite
--     , convention_mainTestGroup
--     , convention_main
    , std
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Data.List -- ( isPrefixOf )
import System.FilePath ( splitPath, takeExtension )

import Test.TBC.Drivers ( Driver(..) )
import Test.TBC.TestSuite

-------------------------------------------------------------------
-- Directory conventions.
-------------------------------------------------------------------

-- FIXME ignore .git, etc.

stdDirectoryConv :: DirectoryConvention s
stdDirectoryConv fulldir s
    | dir `elem` [".darcs", ".git"] = (Skip, s)
    | otherwise = (Cont, s)
  where dir = last (splitPath fulldir)

-------------------------------------------------------------------
-- TestFile conventions.
-------------------------------------------------------------------

stdTestFileConv :: TestFileConvention s
stdTestFileConv f s
    | ext `elem` [".hs", ".lhs"] = (Cont, s)
    | otherwise = (Skip, s)
  where
    ext = takeExtension f

-------------------------------------------------------------------
-- Test conventions.
-------------------------------------------------------------------

-- | The test should yield the string 'True'. This should work for
-- tests of type @Bool@, @IO Bool@, @IO ()@ with a @putStrLn@, ...
booltest :: TestConvention
booltest a@('t':'e':'s':'t':'_':_) = Just run_booltest
  where
    name = mkTestName a

    run_booltest d =
      do r <- hci_send_cmd d $ "seq " ++ name ++ " " ++ name ++ "\n"
         return $ if findTrue r
                    then TestResultSuccess
                    else TestResultFailure r

    findTrue ls = show True == last ls

booltest _ = Nothing

----------------------------------------

-- | The test should throw an exception.
exception :: TestConvention
exception a@('e':'x':'c':'e':'p':'t':'i':'o':'n':_) = Just run_exception
  where
    name = mkTestName a

    run_exception d =
      do r <- hci_send_cmd d $ "seq " ++ name ++ " ()\n"
         return $ if findException r
                    then TestResultSuccess
                    else TestResultFailure r

    findException ls = "*** Exception:" `isPrefixOf` last ls

exception _ = Nothing

----------------------------------------

-- | A HUnit unit test.
hunit :: TestConvention
hunit a@('h':'u':'n':'i':'t':'_':_) = Just run_hunit_all
  where
    name = mkTestName a

    run_hunit_all d = do
           r <- hci_send_cmd d ("seq " ++ name ++ " $ runTestTT $ test " ++ name ++ "\n")
           return $ if findOK r
                      then TestResultSuccess
                      else TestResultFailure r

    findOK ls = "errors = 0, failures = 0" `isInfixOf` last ls

hunit _ = Nothing

----------------------------------------

quickcheck :: TestConvention
quickcheck a@('p':'r':'o':'p':'_':_) = Just run_quickcheck_test
  where
    name = mkTestName a

    run_quickcheck_test d =
      do r <- hci_send_cmd d $ "test " ++ name ++ "\n"
         return $ if findOK r
                    then TestResultSuccess
                    else TestResultFailure r

    -- FIXME what's going on here?
    findOK ls = ", passed" `isInfixOf` last ls

quickcheck _ = Nothing

----------------------------------------

-- | The test should terminate without throwing an exception.
oktest :: TestConvention
oktest a@('o':'k':_) = Just run_oktest
  where
    name = mkTestName a

    run_oktest d =
      do r <- hci_send_cmd d $ name ++ "\n"
         return $ if findException r
                    then TestResultFailure r
                    else TestResultSuccess

    findException ls = "*** Exception:" `isPrefixOf` last ls
oktest _ = Nothing

----------------------------------------

std :: Conventions s
std = Conventions
      { cDirectory = stdDirectoryConv
      , cTestFile = stdTestFileConv
      , cTests = [booltest, exception, hunit, oktest, quickcheck]
      }

{-
FIXME

This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}
