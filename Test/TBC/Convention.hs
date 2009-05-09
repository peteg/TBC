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
 -}
module Test.TBC.Convention
    ( Convention
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

import Data.Char ( isSpace )

import Test.TBC.Conventions ( Convention )
import Test.TBC.TestSuite ( Test(..) )

-------------------------------------------------------------------

-- | FIXME this should follow the Haskell lexical conventions.
mkTestName :: String -> String
mkTestName = takeWhile (not . isSpace)

-------------------------------------------------------------------

hunit :: Convention
hunit a@('t':'e':'s':'t':'_':s) = Just (HUnit { tName = mkTestName s, tAssertion = mkTestName a })
hunit _ = Nothing

----------------------------------------

quickcheck :: Convention
quickcheck a@('p':'r':'o':'p':'_':s) = Just (QuickCheck { tName = mkTestName s, tAssertion = mkTestName a })
quickcheck _ = Nothing

std :: [Convention]
std = [hunit, quickcheck]

{-
This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}
