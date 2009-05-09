{- Test By Convention: Drivers.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.Drivers
    ( tbc
    , tbcCabal
    , tbcWithHooks
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( (>=>) )

import Distribution.PackageDescription
import Distribution.Simple ( Args )
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo

import Test.TBC.Convention as Conv
import Test.TBC.Conventions
import Test.TBC.TestSuite as TestSuite
import Test.TBC.FoldDir ( foldTree )

-------------------------------------------------------------------

-- | FIXME Bells and whistles driver.
tbcWithHooks :: [Convention] -> Config -> FilePath -> IO ()
tbcWithHooks convs config =
    foldTree (conventionalIterator convs) TestSuite.empty
      >=> runTestSuite config
      >=> renderTAP
      >=> putStrLn

-- | FIXME Conventional driver.
tbc :: Config -> FilePath -> IO ()
tbc = tbcWithHooks Conv.std

-- | A driver compatible with Cabal's 'runTests' hook.
-- FIXME assume we're running in the top-level project directory.
tbcCabal :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcCabal _args _wtf pkg_descr localbuildinfo = tbc config "t/"
    where
      config = MkConfig { hc = "ghci" -- FIXME
                        , hc_opts = \f -> f : ghci_opts }
      ghci_opts = ghcOptions localbuildinfo (head (allBuildInfo pkg_descr)) "." -- FIXME allBuildInfo, path "."
