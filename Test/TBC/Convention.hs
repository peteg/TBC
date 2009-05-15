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
    ( hunit
    , quickcheck
--     , convention_mainPlannedTestSuite
--     , convention_mainTestGroup
--     , convention_main
    , std
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Data.Char ( isAlpha, isDigit )

import Test.TBC.Drivers ( Driver(..) )
import Test.TBC.TestSuite ( Convention, Result(..), Test(..) )

-------------------------------------------------------------------

-- | FIXME this should follow the Haskell lexical conventions.
mkTestName :: String -> String
mkTestName = takeWhile (\c -> or (map ($c) [isAlpha, isDigit, ('_' ==)]))

-------------------------------------------------------------------

hunit :: Convention
hunit _f a@('t':'e':'s':'t':'_':_) =
    Just $ Test
             { tName = name
             , tRun = run_hunit_test
             }
  where
    name = mkTestName a

    run_hunit_test d =
      do r <- hci_send_cmd d $ "runTestTT $ TestCase $ " ++ name ++ "\n"
         -- FIXME Grep
         return TestResultSkip
hunit _ _ = Nothing

----------------------------------------

quickcheck :: Convention
quickcheck _f a@('p':'r':'o':'p':'_':_) =
    Just $ Test
             { tName = a
             , tRun = run_quickcheck_test
             }
  where
    name = mkTestName a

    run_quickcheck_test d =
      do r <- hci_send_cmd d $ "test " ++ name ++ "\n"
         -- FIXME Grep
         return TestResultSkip
quickcheck _ _ = Nothing

----------------------------------------

std :: [Convention]
std = [hunit, quickcheck]

{-
FIXME

This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}
