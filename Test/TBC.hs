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

import System.FilePath ( (</>), replaceExtension )
import System.IO.Error -- FIXME

import Distribution.Package ( packageId )
import Distribution.PackageDescription
    ( PackageDescription, allBuildInfo
    , BuildInfo(cSources, extraLibs, extraLibDirs) )
import qualified Distribution.Simple as DS
import Distribution.Simple.BuildPaths ( objExtension )
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir, withPrograms )
import Distribution.Simple.Program ( ghcProgram, lookupProgram, programPath )
import Distribution.Text ( display )

import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.TestSuite as TestSuite

-------------------------------------------------------------------
-- TBC-as-a-library.
-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
tbcWithHooks :: [Convention] -> Renderer -> Driver -> FilePath -> IO ()
tbcWithHooks convs renderer driver testRoot =
    do s@(_run, _succeeded) <- foldTree (conventionalTester convs driver renderer) (0, 0) testRoot
       putStrLn $ renderEnd renderer s
       return ()
    `catch` handler
  where
    handler e = putStrLn ("TBC: " ++ show e)

-- | FIXME Conventional driver.
tbc :: Driver -> FilePath -> IO ()
tbc = tbcWithHooks Conv.std quieterRender

----------------------------------------
-- Cabal support.
----------------------------------------

-- | Drop-in replacement for Cabal's 'Distribution.Simple.defaultMain'.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal "Tests" }

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME assume we're running in the top-level project directory.
-- FIXME generalise to Hugs, etc.
-- FIXME how do we get flags? Verbosity?
tbcCabal :: FilePath -- ^ Where are the tests?
         -> DS.Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal testRoot args _wtf pkg_descr localbuildinfo =
    do let
           -- If the first argument is 'v', be verbose.
           -- FIXME clunky due to a lack of cooperation from Cabal
           verbose = case args of
                       "v":_ -> True
                       _     -> False

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
                             (buildDir localbuildinfo)

       case cmd of
         Nothing -> putStrLn "GHC not found."
         Just hc_cmd ->
           do d <- ghci verbose hc_cmd flags
              tbc d testRoot
              hci_close d
              return ()
