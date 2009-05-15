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

import Control.Monad ( (>=>) )

import Distribution.PackageDescription
import qualified Distribution.Simple as DS
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo

import Test.TBC.Convention as Conv
import Test.TBC.Drivers as Drivers
import Test.TBC.TestSuite as TestSuite
import Test.TBC.FoldDir ( foldTree )

-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
tbcWithHooks :: [Convention] -> Driver -> FilePath -> IO ()
tbcWithHooks convs config =
    foldTree (conventionalIterator convs) TestSuite.empty
      >=> runTestSuite config
--       >=> renderTAP
--       >=> putStrLn

-- | FIXME Conventional driver.
tbc :: Driver -> FilePath -> IO ()
tbc = tbcWithHooks Conv.std

----------------------------------------
-- Cabal support.
----------------------------------------

-- | Drop-in replacement for Cabal's 'Distribution.Simple.defaultMain'.
defaultMain :: IO ()
defaultMain = DS.defaultMainWithHooks hooks
    where hooks = DS.simpleUserHooks { DS.runTests = tbcCabal }

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME assume we're running in the top-level project directory.
-- FIXME join with the process on exit
tbcCabal :: DS.Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal _args _wtf pkg_descr localbuildinfo =
    do d <- ghci cmd flags
       tbc d "t/" -- FIXME generalise
       hci_close d
       return ()
  where
    cmd = "ghci" -- FIXME generalise
    flags = ghcOptions localbuildinfo (head (allBuildInfo pkg_descr)) (buildDir localbuildinfo) -- FIXME allBuildInfo
--      ghci_opts = "-v0" : ghcOptions localbuildinfo (head (allBuildInfo pkg_descr)) (buildDir localbuildinfo) -- FIXME allBuildInfo
