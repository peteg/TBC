{- Test By Convention: test output renderers.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.Renderers
    ( tap
    , quiet
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Test.TBC.TestSuite ( Renderer(..), Result(..), Test(..) )

-------------------------------------------------------------------
-- TAP renderer.
-------------------------------------------------------------------

-- FIXME what do all these fields mean? Some are rubbery.
data TapState
    = TapState
      { tsRun :: !Int
      , tsPassed :: !Int -- ^ Number of tests passed
      , tsToDo :: !Int -- ^ Number of tests run with result 'TestResultToDo'
      , tsTestsSkipped :: !Int -- ^ Number of identified tests that got skipped
      , tsTestFilesSkipped :: !Int -- ^ Number of potential test files that got skipped
      }

tapState0 :: TapState
tapState0 = TapState
            { tsRun = 0
            , tsPassed = 0
            , tsToDo = 0
            , tsTestsSkipped = 0
            , tsTestFilesSkipped = 0
            }

tap :: Renderer TapState
tap =
    Renderer
    { rInitialState = return tapState0
    , rCompilationFailure = tcf
    , rSkip = tskip
    , rStop = tstop
    , rTest = tt
    , rFinal = tf
    }
  where
    tid i t = show i ++ " - " ++ show (tLocation t) ++ " " ++ tName t

    tcf f ts cout s =
      do mapM_ putStrLn $ (("not ok # compilation failed: " ++ f)
                          : cout )
                            ++ ( "# Tests skipped:"
                               : [ "# " ++ tName t | t <- ts ] )
         return s{ tsTestsSkipped = tsTestsSkipped s + length ts
                 , tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tskip f s =
      do putStrLn $ "Skipping " ++ f
         return s{ tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tstop f s =
      do putStrLn $ "Stopping at " ++ f
         return s

    tt t s r =
      case r of
        TestResultFailure strs ->
          do mapM_ putStrLn $ ("not ok " ++ tid i t)
                              : [ '#':' ':l | l <- strs ]
             return s{ tsRun = tsRun s + 1 }
        TestResultSuccess ->
          do putStrLn $ "ok " ++ tid i t
             return s{ tsRun = tsRun s + 1
                     , tsPassed = tsPassed s + 1 }
        TestResultSkip ->
          do putStrLn $ "ok " ++ tid i t ++ " # SKIP FIXME is this OK or not?"
             return s{ tsTestsSkipped = tsTestsSkipped s + 1 }
        TestResultToDo ->
          do putStrLn $ "ok " ++ tid i t
             return s{ tsToDo = tsToDo s + 1 }
      where
        i = tsRun s

    tf s =
      do putStrLn $ "0.." ++ show (tsRun s + tsTestsSkipped s - 1)
         return s

-------------------------------------------------------------------
-- Quiet renderer: only interested in failures.
-------------------------------------------------------------------

-- Only interested in failures.
quiet :: Renderer TapState
quiet =
    Renderer
    { rInitialState = return tapState0
    , rCompilationFailure = tcf
    , rSkip = tskip
    , rStop = tstop
    , rTest = tt
    , rFinal = tf
    }
  where
    tid t = show (tLocation t) ++ " " ++ tName t

    tcf f ts _cout s =
      do putStrLn $ "** Compilation failed: " ++ f
         return s{ tsTestsSkipped = tsTestsSkipped s + length ts
                 , tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tskip f s =
      do putStrLn $ "Skipping " ++ f
         return s{ tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tstop f s =
      do putStrLn $ "Stopping at " ++ f
         return s

    tt t s r =
      case r of
        TestResultFailure strs ->
          do mapM_ putStrLn $ ("** Test failed: " ++ tid t)
                              : [ '#':' ':l | l <- strs ]
             return s{ tsRun = tsRun s + 1 }
        TestResultSuccess ->
             return s{ tsRun = tsRun s + 1
                     , tsPassed = tsPassed s + 1 }
        TestResultSkip ->
          do putStrLn $ "ok " ++ tid t ++ " # SKIP FIXME is this OK or not?"
             return s{ tsTestsSkipped = tsTestsSkipped s + 1 }
        TestResultToDo ->
          do putStrLn $ "ok " ++ tid t
             return s{ tsToDo = tsToDo s + 1 }

    tf s =
      do putStrLn $ "Passed " ++ show (tsPassed s) ++ " / " ++ show (tsRun s)
                       ++ skipped
         return s
      where
        skipped
            | tsTestsSkipped s == 0 = ""
            | otherwise = " (Skipped " ++ show (tsTestsSkipped s) ++ ")"
