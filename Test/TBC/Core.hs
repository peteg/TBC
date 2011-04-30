{- Test By Convention: core types and functions.
 - Copyright   :  (C)opyright 2009-2011 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.Core
    ( DirectoryConvention
    , TestFileConvention
    , TestConvention
    , Action(..)
    , Test(..)
    , Result(..)
    , Renderer
    , RenderFns(..)
    , Conventions(..)

    , Verbosity
    , silent, normal, verbose, deafening
    , warn, notice, setupMessage, info, debug

    , Location(..)
    , mkLocation
    , mkTestName

    , traverseDirectories
    , applyTestConventions
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM, foldM )

import Data.Char ( isAlpha, isDigit, isSpace )
import Data.List ( nubBy )
import Data.Maybe ( catMaybes )

import Distribution.Simple.Utils ( warn, notice, setupMessage, info, debug )
import Distribution.Verbosity ( Verbosity, silent, normal, verbose, deafening )

import System.Directory ( Permissions(searchable), getDirectoryContents, getPermissions )
import System.Exit ( ExitCode )
import System.FilePath ( (</>) )

import Test.TBC.Drivers ( Driver(hci_load_file) )

-------------------------------------------------------------------
-- Individual tests.
-------------------------------------------------------------------

-- | Location of a 'Test'.
data Location
    = Location
      { lFile :: FilePath
      , lLine :: Int
      , lColumn :: Int
      }

mkLocation :: FilePath -> Int -> Int -> Location
mkLocation = Location

instance Show Location where
    show l = lFile l ++ ":" ++ show (lLine l) ++ ":" ++ show (lColumn l)

-- | Discern a test name from a string, viz the entirety of the varid
-- starting at the start of the string. FIXME this should follow the
-- Haskell lexical conventions and perhaps be more robust.
mkTestName :: String -> String
mkTestName = takeWhile (\c -> or (map ($c) [ isAlpha, isDigit, (`elem` ['_', '\'']) ])) . unlit
  where
    unlit ('>':cs) = dropWhile isSpace cs
    unlit str      = str

-- | A single test.
data Test
    = Test
      { tName :: String -- ^ Each 'Test' in a 'TestFile' must have a different name.
      , tLocation :: Location
      , tRun :: Driver -> IO Result
      }

-- | The result of a single 'Test'.
data Result
    = TestResultSkip -- ^ FIXME Skip this test
    | TestResultToDo -- ^ This test is not yet done
    | TestResultStop -- ^ FIXME Stop testing
    | TestResultSuccess
    | TestResultFailure { msg :: [String] }
      deriving (Show)

-------------------------------------------------------------------
-- Test output renderers.
-------------------------------------------------------------------

-- | A renderer maps a verbosity level into a bunch of functions that
-- tells the user of various events.
type Renderer s = Verbosity -> RenderFns s

data RenderFns s
    = RenderFns
      { rInitialState :: IO s
      , rCompilationFailure :: FilePath -- ^ TestFile
                            -> [Test] -- ^ Tests in the file
                            -> [String] -- ^ Output from the Haskell system
                            -> s
                            -> IO s
      , rSkip :: FilePath -> s -> IO s -- FIXME refine: skipped a file, skipped some tests, some tests told us to skip, ...
      , rStop :: FilePath -> s -> IO s
      , rTest :: Test
              -> s
              -> Result
              -> IO s
      , rFinal :: s -> IO ExitCode
      }

-------------------------------------------------------------------
-- Conventions.
-- FIXME some might like some IO's sprinkled in here.
-------------------------------------------------------------------

-- | FIXME
data Action = Stop | Skip | Cont

-- | A /directory convention/ maps a directory name into an action.
type DirectoryConvention s = FilePath -> s -> (Action, s)

-- | A /test file convention/ maps a file name into an action.
type TestFileConvention s = FilePath -> s -> (Action, s)

-- | A /test convention/ maps a line in a 'TestFile' into a function
-- that runs the test.
type TestConvention = String -> Maybe (Driver -> IO Result)

-- | A collection of conventions.
data Conventions s
    = Conventions
      { cDirectory :: DirectoryConvention s
      , cTestFile :: TestFileConvention s
      , cTests :: [TestConvention]
      }

-------------------------------------------------------------------
-- Directory traversal.
-------------------------------------------------------------------

-- | Visit all files in a directory tree.
-- FIXME try to eliminate the "." with some refactoring.
traverseDirectories :: Conventions s -> Driver -> RenderFns s -> [FilePath] -> s -> IO s
traverseDirectories convs driver renderer paths s0 = snd `liftM` walk s0 "." paths
  where
    fold s path =
      case cDirectory convs path s of
        (Cont, s') -> getUsefulContents path >>= walk s' path
        (Skip, s') -> rSkip renderer path s >> return (Cont, s')
        as'@(Stop, _s') -> rStop renderer path s >> return as'

    walk s _ [] = return (Cont, s)
    walk s path (name:names) =
      do let path' = path </> name
         perms <- getPermissions path'
         as'@(a, s') <-
             if searchable perms
               then fold s path' -- It's a directory, Jim.
               else testFile convs driver renderer s path' -- It's a file.
         case a of
           Cont -> walk s' path names
           _    -> return as'

    getUsefulContents :: FilePath -> IO [String]
    getUsefulContents p =
        filter (`notElem` [".", ".."]) `liftM` getDirectoryContents p

-- | Execute all tests in a given test file, if it passes the
-- 'cTestFile' convention.
testFile :: Conventions s -> Driver -> RenderFns s -> s -> FilePath -> IO (Action, s)
testFile convs driver renderer s0 f =
    case cTestFile convs f s0 of
      as'@(Stop, _s) -> return as' -- Stop testing.
      (Skip, s)      ->
        do _ <-rSkip renderer f s
           return (Cont, s) -- ... but continue testing.
      (Cont, s)      ->
        do -- putStrLn $ "Running: " ++ f
           ts <- applyTestConventions (cTests convs) f `liftM` readFile f
           mCout <- hci_load_file driver f
           s' <- case mCout of
                   [] -> foldM runTest s ts
                   cout -> rCompilationFailure renderer f ts cout s
           return (Cont, s')
  where
    runTest s t = tRun t driver >>= rTest renderer t s

{-
This logic requires more work:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}

-- | Apply a list of conventions to the guts of a 'TestFile'.
applyTestConventions :: [TestConvention] -> FilePath -> String -> [Test]
applyTestConventions cs f = nubBy (eqOn tName) . catMaybes . applyCs . lines
  where
    applyCs ls = [ mkTest l lineNum `fmap` c l | (l, lineNum) <- zip ls [1..], c <- cs ]
    eqOn p x y = p x == p y

    mkTest l lineNum trun =
        Test { tName = mkTestName l
             , tLocation = mkLocation f lineNum 0
             , tRun = trun
             }
