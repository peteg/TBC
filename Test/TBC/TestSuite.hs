{- Test By Convention: The TestSuite type and friends.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.TestSuite
    ( Convention
    , Test(..)
    , Result(..)
    , Renderer

    , foldTree

    , applyConventions
    , conventionalTester
    , tapRender
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM, when )

import Data.Maybe ( catMaybes )
import Data.List ( isInfixOf, nub )

import System.Directory ( Permissions(searchable), getDirectoryContents, getPermissions )
import System.FilePath ( (</>) )

import Test.TBC.Drivers ( Driver(hci_send_cmd) )

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | A /convention/ maps a line in a 'TestFile' into a function that
-- runs the test.
type Convention = FilePath -> String -> Maybe Test

type Renderer = Test -> Result -> String

-- | A single test.
-- FIXME probably needs line numbers sprinkled all through this
data Test
    = Test
      { tName :: String
      , tRun :: Driver -> IO Result
      }

instance Eq Test where
    t == t' = tName t == tName t'

-- | The result of a single 'Test'.
data Result
    = TestResultSkip
    | TestResultToDo
    | TestResultSuccess
    | TestResultFailure
    | TestResultQuickCheckCounterExample
      deriving (Show)

-------------------------------------------------------------------

data ItResult = Continue | Done
                deriving (Show)

type Iterator a = a -> FilePath -> IO (ItResult, a)

-- | Visit all files in a directory tree. Note we don't invoke the
-- iterator on directories, only on files.
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = snd `liftM` fold initSeed path
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath

    walk seed _ [] = return (Continue, seed)
    walk seed subpath (name:names) =
      do let path' = subpath </> name
         perms <- getPermissions path'
         rs@(r, seed') <- if searchable perms
                            then fold seed path' -- It's a directory, Jim.
                            else iter seed path' -- It's a file.
         case r of
           Continue -> walk seed' subpath names
           Done     -> return rs

    getUsefulContents :: FilePath -> IO [String]
    getUsefulContents p =
        filter (`notElem` [".", ".."]) `liftM` getDirectoryContents p

-------------------------------------------------------------------

-- | Apply a list of conventions to the guts of a 'TestFile'.
applyConventions :: [Convention] -> FilePath -> String -> [Test]
applyConventions cs f = catMaybes . applyCs . lines
    where applyCs ls = [ c f l | l <- ls, c <- cs ]

conventionalTester :: [Convention] -> Driver -> Renderer -> Iterator ()
conventionalTester cs driver renderer _ f =
  do putStrLn $ "conventionIterator: " ++ f
     ts <- applyConventions cs f `liftM` readFile f
     cout <- hci_send_cmd driver load_file
     when (not ("Ok, modules loaded" `isInfixOf` last cout)) $
       error "Compilation problems." -- FIXME
     mapM_ runTest (nub ts)
     return (Continue, ()) -- FIXME
  where
    load_file = ":l " ++ f ++ "\n"

    runTest t =
      do r <- tRun t driver
         putStrLn (renderer t r)

{-
This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}

-------------------------------------------------------------------

tapRender :: Test -> Result -> String
tapRender = error "renderTAP"
