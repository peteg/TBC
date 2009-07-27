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

import Test.TBC.Core ( Renderer, RenderFns(..), Result(..), Test(..)
                     , info )

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
      , tsCompilationFailures :: !Int -- ^ Number of test files that failed to compile
      }

tapState0 :: TapState
tapState0 = TapState
            { tsRun = 0
            , tsPassed = 0
            , tsToDo = 0
            , tsTestsSkipped = 0
            , tsTestFilesSkipped = 0
            , tsCompilationFailures = 0
            }

tap :: Renderer TapState
tap verbosity =
    RenderFns
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
         return s{ tsCompilationFailures = tsCompilationFailures s + 1 }

    tskip f s =
      do info verbosity $ "Skipping " ++ f
         return s{ tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tstop f s =
      do putStrLn $ "Stopping at " ++ f
         return s

    tt t s r =
      case r of
        TestResultSkip ->
          do putStrLn $ "ok " ++ tid i t ++ " # SKIP FIXME is this OK or not?"
             return s{ tsTestsSkipped = tsTestsSkipped s + 1 }
        TestResultToDo ->
          do putStrLn $ "ok " ++ tid i t
             return s{ tsToDo = tsToDo s + 1 }
        TestResultStop ->
          do putStrLn $ "ok " ++ tid i t ++ " # STOP FIXME is this OK or not?"
             return s -- FIXME ????
        TestResultFailure strs ->
          do mapM_ putStrLn $ ("not ok " ++ tid i t)
                              : [ '#':' ':l | l <- strs ]
             return s{ tsRun = tsRun s + 1 }
        TestResultSuccess ->
          do putStrLn $ "ok " ++ tid i t
             return s{ tsRun = tsRun s + 1
                     , tsPassed = tsPassed s + 1 }
      where
        i = tsRun s

    tf s =
      do putStrLn $ "0.." ++ show (tsRun s + tsTestsSkipped s - 1)
         return s

-------------------------------------------------------------------

-- | UNIX style: only reports failures.
quiet :: Renderer TapState
quiet verbosity =
    RenderFns
    { rInitialState = return tapState0
    , rCompilationFailure = tcf
    , rSkip = tskip
    , rStop = tstop
    , rTest = tt
    , rFinal = tf
    }
  where
    tid t = show (tLocation t) ++ " " ++ tName t

    tcf f _ts _cout s =
      do putStrLn $ "** Compilation failed: " ++ f
         return s{ tsCompilationFailures = tsCompilationFailures s + 1 }

    tskip f s =
      do info verbosity $ "Skipping " ++ f
         return s{ tsTestFilesSkipped = tsTestFilesSkipped s + 1 }

    tstop f s =
      do putStrLn $ "Stopping at " ++ f
         return s

    -- FIXME in a big way
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
        TestResultStop ->
          do putStrLn $ "ok " ++ tid t ++ " # STOP FIXME is this OK or not?"
             return s -- FIXME ????

    tf s =
      do putStrLn $ "Passed " ++ show (tsPassed s) ++ " / " ++ show (tsRun s)
                       ++ skipped ++ cfail
         return s
      where
        cfail
            | tsCompilationFailures s == 0 = ""
            | otherwise = " (Failed to compile " ++ show (tsCompilationFailures s) ++ ")"

        skipped
            | tsTestsSkipped s == 0 = ""
            | otherwise = " (Skipped " ++ show (tsTestsSkipped s) ++ ")"
