{- Test By Convention: Top-level drivers.
 - Copyright   :  (C)opyright 2009-2012 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC
    ( -- * Conventions and data structures.
      module Test.TBC.Convention
    , module Test.TBC.Core
    , module Test.TBC.Drivers

      -- * Top-level drivers.
    , tbc
    , tbcWithHooks
    , tbcCabal
    , defaultMain
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Prelude hiding ( catch )
import Control.Exception ( catch, SomeException )

import System.Exit ( ExitCode(ExitFailure), exitFailure, exitWith )
import System.FilePath ( (</>), replaceExtension )
import System.Posix.Signals ( installHandler, sigINT, Handler(..) )

import Distribution.Package ( packageId )
import Distribution.PackageDescription
    ( PackageDescription, allBuildInfo
    , BuildInfo(cSources, extraLibs, extraLibDirs) )
import qualified Distribution.Simple as DS -- FIXME update
import Distribution.Simple.BuildPaths ( objExtension )
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir, withLibLBI, withPrograms )
import Distribution.Simple.Program ( ghcProgram, lookupProgram, programPath )
import Distribution.Text ( display )

-- FIXME This is what we want to say:
import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.Renderers as Renderers
import Test.TBC.Core as Core
-- ... but Haddock doesn't understand (Haskell Platform 2012.2.0.0), so...
import Test.TBC.Convention
import Test.TBC.Drivers
import Test.TBC.Core

-------------------------------------------------------------------
-- TBC-as-a-library.
-------------------------------------------------------------------

-- | A parametrised bells-and-whistles driver.
tbcWithHooks :: Conventions s -> RenderFns s -> Driver -> [FilePath] -> IO ExitCode
tbcWithHooks convs renderer driver testRoots =
  (      rInitialState renderer
     >>= traverseDirectories convs driver renderer testRoots
     >>= rFinal renderer
  ) `catch` handler
  where
    handler :: SomeException -> IO ExitCode
    handler e = putStrLn ("TBC: " ++ show e) >> return (ExitFailure 1)

-- | A hardwired (conventional) driver.
tbc :: Driver -> [FilePath] -> IO ()
tbc driver testRoots =
       tbcWithHooks Conv.std (Renderers.quiet Core.normal) driver testRoots
    >> return ()

----------------------------------------
-- Cabal support.
----------------------------------------

-- | This is a drop-in replacement for Cabal's
-- 'Distribution.Simple.defaultMain'.
--
-- However the test infrastructure in Cabal has changed since this was
-- written, and its use is discouraged. Use the TBC binary instead.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal normal }

-- | A driver compatible with Cabal's 'runTests' hook.
--
-- However the test infrastructure in Cabal has changed since this was
-- written, and its use is discouraged. Use the TBC binary instead.
--
-- This is used by the TBC binary.
tbcCabal :: Verbosity
         -> DS.Args -- ^ Where are the tests (dirs and files)?
         -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal verbosity args _wtf pkg_descr localbuildinfo =
    cabalDriver verbosity args pkg_descr localbuildinfo >> return ()

-- | Core Cabal-based driver.
-- FIXME generalise to Hugs, etc.
-- FIXME withLibLBI should use IO a, not IO (). Hack around it for
-- now: this function exits.
cabalDriver :: Verbosity -> DS.Args -> PackageDescription -> LocalBuildInfo -> IO ()
cabalDriver verbosity args pkg_descr localbuildinfo =
  withLibLBI pkg_descr localbuildinfo $ \_lib clbi ->
    do let
           testRoots
               | null args = ["Tests"]
               | otherwise = args

           -- Find GHC
           cmd = fmap programPath (lookupProgram ghcProgram (withPrograms localbuildinfo))

           -- The tests are part of the package (from GHC's pov).
           pkgid = packageId pkg_descr

           -- FIXME We only test the first thing.
           buildInfo = head (allBuildInfo pkg_descr)

           -- FIXME hardwire the path?
           -- This requires that the user invoked "Setup build".
           cObjs = [ buildDir localbuildinfo </> c `replaceExtension` objExtension
                     | c <- cSources buildInfo ]

           flags =
             ["-v1", "--interactive", "-package-name", display pkgid ]
               ++ [ "-l" ++ extraLib | extraLib <- extraLibs buildInfo ]
               ++ [ "-L" ++ extraLibDir | extraLibDir <- extraLibDirs buildInfo ]
               ++ cObjs
               ++ ghcOptions localbuildinfo
                             buildInfo
                             clbi
                             (buildDir localbuildinfo)

       case cmd of
         Nothing -> putStrLn "GHC not found."
         Just hc_cmd ->
           do driver <- ghci verbosity hc_cmd flags

              -- TODO arguably other signals too
              -- TODO timeouts: although perhaps bad idea to arbitrarily limit time for a test run
              -- TODO windows: now we need to import unix package for System.Posix.Signals
              _ <- installHandler sigINT (Catch $ do
                                             hci_kill driver
                                             exitFailure
                                         ) Nothing

              exitCode <- tbcWithHooks Conv.std (Renderers.quiet Core.normal) driver testRoots
              _ <- hci_close driver
              exitWith exitCode -- FIXME hack around Cabal's restrictive types.
