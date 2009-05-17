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

import Distribution.Package ( packageId )
import Distribution.PackageDescription ( PackageDescription, allBuildInfo )
import qualified Distribution.Simple as DS
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir, withPrograms )
import Distribution.Simple.Program ( ghcProgram, lookupProgram, programPath )
import Distribution.Text ( display )

import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.TestSuite as TestSuite

-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
tbcWithHooks :: [Convention] -> Renderer -> Driver -> FilePath -> IO ()
tbcWithHooks convs renderer driver f =
  do s@(_run, _succeeded) <- foldTree (conventionalTester convs driver renderer) (0, 0) f
     putStrLn $ renderEnd renderer s
     return ()

-- | FIXME Conventional driver.
tbc :: Driver -> FilePath -> IO ()
tbc = tbcWithHooks Conv.std quietRender

----------------------------------------
-- Cabal support.
----------------------------------------

-- | Drop-in replacement for Cabal's 'Distribution.Simple.defaultMain'.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal }

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME assume we're running in the top-level project directory.
-- FIXME generalise to Hugs, etc.
tbcCabal :: DS.Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal _args _wtf pkg_descr localbuildinfo =
    do case cmd of
         Nothing -> putStrLn "GHC not found."
         Just hc_cmd ->
           do d <- ghci hc_cmd flags
              tbc d "Tests/" -- FIXME generalise
              hci_close d
              return ()
  where
    cmd = fmap programPath (lookupProgram ghcProgram (withPrograms localbuildinfo))

    -- The tests are part of the package.
    pkgid = packageId pkg_descr

    flags = "-v1"
              : "--interactive"
              : "-package-name" : display pkgid
              : ghcOptions localbuildinfo (head (allBuildInfo pkg_descr)) (buildDir localbuildinfo) -- FIXME allBuildInfo
