{- Test By Convention: Top-level drivers.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC
    ( -- * FIXME Conventions, data structures.
      module Conv
    , module Drivers
    , module TestSuite

      -- * Top-level drivers.
    , tbc
    , tbcWithHooks
    , tbcCabal
    , defaultMain
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import System.Exit ( exitFailure )
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
import Distribution.Verbosity ( Verbosity, normal )

import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.Renderers as Renderers
import Test.TBC.TestSuite as TestSuite

-------------------------------------------------------------------
-- TBC-as-a-library.
-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
-- FIXME invoke the renderer functions appropriately.
tbcWithHooks :: Conventions s -> Renderer s -> Driver -> [FilePath] -> IO ()
tbcWithHooks convs renderer driver testRoots =
  (      rInitialState renderer
     >>= traverseDirectories convs driver renderer testRoots
     >>= rFinal renderer
     >>  return ()
  ) `catch` handler
  where
    handler e = putStrLn ("TBC: " ++ show e)

-- | FIXME Conventional driver.
tbc :: Driver -> [FilePath] -> IO ()
tbc = tbcWithHooks Conv.std Renderers.quiet

----------------------------------------
-- Cabal support.
----------------------------------------

-- | Drop-in replacement for Cabal's 'Distribution.Simple.defaultMain'.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal normal }

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME assume we're running in the top-level project directory.
-- FIXME generalise to Hugs, etc.
-- FIXME how do we get flags? Verbosity?
tbcCabal :: Verbosity
         -> DS.Args -- ^ Where are the tests (dirs and files)?
         -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal verbosity args _wtf pkg_descr localbuildinfo =
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
           do d <- ghci verbosity hc_cmd flags

              -- TODO arguably other signals too
              -- TODO timeouts: although perhaps bad idea to arbitrarily limit time for a test run
              -- TODO windows: now we need to import unix package for System.Posix.Signals
              installHandler sigINT (Catch $ do
                                       hci_kill d
                                       exitFailure
                                    ) Nothing

              tbc d testRoots
              hci_close d
              return ()



