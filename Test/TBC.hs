{- Test By Convention: Top-level drivers.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC
    ( -- * FIXME Conventions, data structures.
      module Conv
    , module Core
    , module Drivers

      -- * Top-level drivers.
    , tbc
    , tbcWithHooks
    , tbcCabal
    , defaultMain
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import System.Exit ( ExitCode(ExitFailure), exitFailure, exitWith )
import System.FilePath ( (</>), replaceExtension )
import System.IO.Error -- FIXME
import System.Posix.Signals ( installHandler, sigINT, Handler(..) )

import Distribution.Package ( packageId )
import Distribution.PackageDescription
    ( PackageDescription, allBuildInfo
    , BuildInfo(cSources, extraLibs, extraLibDirs) )
import qualified Distribution.Simple as DS
import Distribution.Simple.BuildPaths ( objExtension )
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir, withLibLBI, withPrograms )
import Distribution.Simple.Program ( ghcProgram, lookupProgram, programPath )
import Distribution.Text ( display )

import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.Renderers as Renderers
import Test.TBC.Core as Core

-------------------------------------------------------------------
-- TBC-as-a-library.
-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
-- FIXME invoke the renderer functions appropriately.
tbcWithHooks :: Conventions s -> RenderFns s -> Driver -> [FilePath] -> IO ExitCode
tbcWithHooks convs renderer driver testRoots =
  (      rInitialState renderer
     >>= traverseDirectories convs driver renderer testRoots
     >>= rFinal renderer
  ) `catch` handler
  where
    handler e = putStrLn ("TBC: " ++ show e) >> return (ExitFailure 1)

-- | FIXME Conventional driver.
tbc :: Driver -> [FilePath] -> IO ()
tbc driver testRoots =
       tbcWithHooks Conv.std (Renderers.quiet Core.normal) driver testRoots
    >> return ()

----------------------------------------
-- Cabal support.
----------------------------------------

-- | Drop-in replacement for Cabal's 'Distribution.Simple.defaultMain'.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal normal }

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME generalise to Hugs, etc.
-- FIXME how do we get flags? Verbosity?
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
              installHandler sigINT (Catch $ do
                                       hci_kill driver
                                       exitFailure
                                    ) Nothing

              exitCode <- tbcWithHooks Conv.std (Renderers.quiet Core.normal) driver testRoots
              hci_close driver
              exitWith exitCode -- FIXME hack around Cabal's restrictive types.
