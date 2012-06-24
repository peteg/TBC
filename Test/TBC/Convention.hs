{- | Test By Convention: the conventions themselves.
 - Copyright   :  (C)opyright 2009-2012 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -
 - Idea is to apply each of these tests to each line of a 'TestFile'
 - and collate the resulting 'TestSuite'.
 -
 - FIXME Import qualified.
 - FIXME tests that appear in block comments {- -} are still picked up.
 - FIXME we'd really like an EDSL here.
 -}
module Test.TBC.Convention
    ( booltest
    , exception
    , hunit
    , quickcheck
--     , convention_mainPlannedTestSuite
--     , convention_mainTestGroup
--     , convention_main
    , stdDirectoryConv, stdTestFileConv, std
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Data.List -- ( isPrefixOf )
import Data.Char (isSpace)
import System.FilePath ( splitPath, takeExtension, takeFileName )

import Test.TBC.Drivers ( Driver(..) )
import Test.TBC.Core

-------------------------------------------------------------------
-- Directory conventions.
-------------------------------------------------------------------

-- | Skip @.darcs@ and @.git@ directories, and Cabal's @dist@
-- directory.
--
-- Could also imagine skipping subproject directories.
stdDirectoryConv :: DirectoryConvention s
stdDirectoryConv fulldir s
    | dir `elem` [".darcs", ".git", "dist"] = (Skip, s)
    | otherwise = (Cont, s)
  where dir = last (splitPath fulldir)

-------------------------------------------------------------------
-- TestFile conventions.
-------------------------------------------------------------------

-- | Skip Cabal's @Setup.hs@.
stdTestFileConv :: TestFileConvention s
stdTestFileConv f s
    | takeFileName f == "Setup.hs" = (Skip, s)
    | ext `elem` [".hs", ".lhs"] = (Cont, s)
    | otherwise = (Skip, s)
  where
    ext = takeExtension f

-------------------------------------------------------------------
-- Test conventions.
-------------------------------------------------------------------

findException :: [String] -> Bool
findException [] = False
findException ls = "*** Exception:" `isPrefixOf` last ls
                   || "_exception ::" `isPrefixOf` last ls

findTrue :: [String] -> Bool
findTrue [] = False
findTrue ls = show True == last ls

----------------------------------------

-- | The test should yield the string 'True'. This should work for
-- tests of type @Bool@, @IO Bool@, @IO ()@ with a @putStrLn@, ...
--
-- Note the 'seq' in its implementation is not entirely useless: the
-- test may use 'unsafePerformIO' or 'trace' to incidentally output
-- things after 'True'.
booltest :: TestConvention
booltest a@('t':'e':'s':'t':'_':_) = Just run_booltest
  where
    name = mkTestName a

    run_booltest d =
      do r <- hci_send_cmd d $ "seq " ++ name ++ " " ++ name ++ "\n"
         return $ if findTrue r
                    then TestResultSuccess
                    else TestResultFailure r

booltest _ = Nothing

----------------------------------------

-- | The 'seq'ed test should throw an exception.
exception :: TestConvention
exception a@('e':'x':'c':'e':'p':'t':'i':'o':'n':_) = Just run_exception
  where
    name = mkTestName a

    run_exception d =
      do r <- hci_send_cmd d $ "seq " ++ name ++ " ()\n"
         return $ if findException r
                    then TestResultSuccess
                    else TestResultFailure r

exception _ = Nothing

----------------------------------------

-- | The @deepseq@'d test should throw an exception.
deep_exception :: TestConvention
deep_exception a@('d':'e':'e':'p':'_':'e':'x':'c':'e':'p':'t':'i':'o':'n':_) = Just run_exception
  where
    name = mkTestName a

    run_exception d =
      do r <- hci_send_cmd d $ "Control.DeepSeq.deepseq " ++ name ++ " ()\n"
         return $ if findException r
                    then TestResultSuccess
                    else TestResultFailure r

deep_exception _ = Nothing

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

    findOK [] = False
    findOK ls = "errors = 0, failures = 0" `isInfixOf` last ls

hunit _ = Nothing

----------------------------------------

-- | A QuickCheck test. We use the 'Test.QuickCheck.quickCheck'
-- driver, i.e., the default settings.
quickcheck :: TestConvention
quickcheck a@('p':'r':'o':'p':'_':_) = Just run_quickcheck_test
  where
    name = mkTestName a

    run_quickcheck_test d =
      do r <- hci_send_cmd d $ "Test.QuickCheck.quickCheck " ++ name ++ "\n"
         return $ if findOK r
                    then TestResultSuccess
                    else TestResultFailure r

    -- FIXME what's going on here?
    findOK [] = False
    findOK ls = ", passed" `isInfixOf` last ls

quickcheck _ = Nothing

----------------------------------------

-- | The @seq@'d test should terminate without throwing an exception.
oktest :: TestConvention
oktest a@('o':'k':_) = Just run_oktest
  where
    name = mkTestName a

    run_oktest d =
      do r <- hci_send_cmd d $ "seq " ++ name ++ " True\n"
         return $ if findTrue r
                    then TestResultSuccess
                    else TestResultFailure r

oktest _ = Nothing

----------------------------------------

-- | The @deepseq@'d test should terminate without throwing an exception.
deep_oktest :: TestConvention
deep_oktest a@('d':'e':'e':'p':'_':'o':'k':_) = Just run_oktest
  where
    name = mkTestName a

    run_oktest d =
      do r <- hci_send_cmd d $ "Control.DeepSeq.deepseq " ++ name ++ " True\n"
         return $ if findTrue r
                    then TestResultSuccess
                    else TestResultFailure r

deep_oktest _ = Nothing

----------------------------------------

-- | The standard set of conventions.
std :: Conventions s
std = Conventions
      { cDirectory = stdDirectoryConv
      , cTestFile = stdTestFileConv
      , cTests = map unlittest tests
      }
  where
    -- We might need to remove bird tracks from an lhs file.
    unlittest :: TestConvention -> TestConvention
    unlittest c ('>':ln) = c $ dropWhile isSpace ln
    unlittest c ln       = c ln

    tests = [booltest, deep_exception, exception, hunit, deep_oktest, oktest, quickcheck]

{-
FIXME

This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}
