{- Test By Convention, executable
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module TBC ( main ) where

import Tests.TBC ( tbcCabal )

{-

Search upwards until we find BLAH.cabal

Load that and use the options within.

-}

main :: IO ()
main = _ <- findPackageDesc "."
