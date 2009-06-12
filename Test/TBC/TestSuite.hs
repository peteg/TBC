{- Test By Convention: The TestSuite type and friends.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.TestSuite
    ( Convention
    , Test(..)
    , Result(..)
    , Renderer(..)

    , foldTree

    , applyConventions
    , conventionalTester

    , tapRender
    , quietRender
    , quieterRender
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM, foldM )

import Data.List ( nubBy )
import Data.Maybe ( catMaybes )

import System.Directory ( Permissions(searchable), getDirectoryContents, getPermissions )
import System.FilePath ( (</>), takeExtension )

import Test.TBC.Drivers ( Driver(hci_load_file) )

-------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------

-- | A /convention/ maps a line in a 'TestFile' into a function that
-- runs the test.
type Convention = FilePath -> String -> Maybe Test

-- | A single test.
-- FIXME probably needs line numbers sprinkled all through this
data Test
    = Test
      { tName :: String -- ^ Each 'Test' in a 'TestFile' must have a different name.
      , tRun :: Driver -> IO Result
      }

-- | The result of a single 'Test'.
data Result
    = TestResultSkip
    | TestResultSkipRestOfDirectory
    | TestResultToDo
    | TestResultSuccess
    | TestResultFailure { msg :: [String] }
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

conventionalTester :: [Convention] -> Driver -> Renderer -> Iterator (Int, Int)
conventionalTester cs driver renderer s@(run, succeeded) f =
  if ext `elem` [".hs", ".lhs"] -- FIXME abstract
    then do -- putStrLn $ "conventionIterator: " ++ f
            ts <- applyConventions cs f `liftM` readFile f
            mCout <- hci_load_file driver f
            s' <- case mCout of
                    [] -> foldM runTest s (nubBy (eqOn tName) ts)
                    cout -> do putStrLn $ renderCompilationFailure renderer s f ts cout
                               return (run + 1, succeeded)
            return (Continue, s') -- FIXME can tests stop the traversal?
    else do putStrLn $ "Skipping: " ++ f
            return (Continue, s)
  where
    ext = takeExtension f

    eqOn p x y = p x == p y

    runTest (run', succeeded') t =
      do r <- tRun t driver
         putStr (renderTest renderer run' f t r)
         return ( run' + 1
                , case r of
                    TestResultSuccess -> succeeded' + 1
                    _                 -> succeeded'
                )

{-
This logic requires an overhaul of the types:

  - if you define mainPlannedTestSuite :: (Plan Int, IO TestSuiteResult), we assume you need control and we'll run it and merge the TAP with other tests. (also mainTestSuite)
  - elsif you define mainTestGroup :: (Plan Int, IO TestGroupResult), we assume you need control and we'll run it and merge the TAP with other tests.
  - elsif you define main :: IO (), we'll treat it as a single test that's passed if it compiles and runs without an exception (?) -- quick and dirty.
-}

-------------------------------------------------------------------

-- Renderers
-- FIXME Separate module?
-- FIXME parameterise by verbosity. Perhaps only need the TAP one then.

-- | FIXME A renderer...
-- Convention: responsible for putting in all the newlines.
-- FIXME needs to provide a state type.
data Renderer
    = Renderer
      { renderBegin :: String
      , renderTest :: Int -- ^ Global test number
                   -> FilePath -- ^ TestFile
                   -> Test
                   -> Result
                   -> String
      , renderCompilationFailure :: (Int, Int) -- ^ Global test number FIXME
                                 -> FilePath -- ^ TestFile
                                 -> [Test] -- ^ Tests in the file
                                 -> [String] -- ^ Output from the Haskell system
                                 -> String
      , renderEnd :: (Int, Int) -> String
      }

tapRender :: Renderer
tapRender =
    Renderer
    { renderBegin = ""
    , renderTest = tapR
    , renderCompilationFailure = tapCF
    , renderEnd = tapE
    }
  where
    tid i f t = show i ++ " - " ++ f ++ ":" ++ tName t

    tapR i f t r =
        case r of
          TestResultFailure strs -> "not ok " ++ tid i f t ++ "\n" ++ rFail strs
          TestResultSuccess -> "ok " ++ tid i f t ++ "\n"
        where
          rFail strs = unlines [ '#':' ':s | s <- strs ]

    tapCF i f ts cout =
        "not ok # compilation failed: " ++ f ++ "\n"
      ++ unlines (cout ++ ["# Tests skipped:"] ++ [ "# " ++ tName t | t <- ts ])

    tapE (run, _succeeded) = "0.." ++ show (run - 1)

----------------------------------------

quietRender :: Renderer
quietRender =
    Renderer
    { renderBegin = ""
    , renderTest = quietR
    , renderCompilationFailure = quietCF
    , renderEnd = quietE
    }
  where
    quietR i f t r =
        case r of
          TestResultFailure strs -> "not ok " ++ tid ++ "\n" ++ rFail strs
          TestResultSuccess -> ""
        where
          tid = show i ++ " - " ++ f ++ ":" ++ tName t
          rFail strs = unlines [ '#':' ':s | s <- strs ]

    quietCF i f ts cout =
        "** Compilation failed: " ++ f ++ "\n"
      ++ unlines (rCout ++ if (null ts) then [] else ["", "** Tests skipped:"] ++ [ ' ' : tName t | t <- ts ])
        where
          rCout = [ '#':' ':s | s <- cout ]

    quietE (run, succeeded) = "Passed " ++ show succeeded ++ " / " ++ show run

----------------------------------------

-- Only interested in failures.
quieterRender :: Renderer
quieterRender =
    Renderer
    { renderBegin = ""
    , renderTest = quietR
    , renderCompilationFailure = quietCF
    , renderEnd = quietE
    }
  where
    quietR i f t r =
        case r of
          TestResultFailure strs -> "Failed: " ++ tid ++ "\n"
          TestResultSuccess -> ""
        where
          tid = show i ++ " - " ++ f ++ ":" ++ tName t

    quietCF i f ts cout =
        "** Compilation failed: " ++ f

    quietE (run, succeeded) = "Passed " ++ show succeeded ++ " / " ++ show run
