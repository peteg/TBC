{- Test By Convention: FoldDir from Real World Haskell.
 - Copyright   :  FIXME dons et al (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -
 - http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
 -}
module Test.TBC.FoldDir
    (
      ItResult(..)
    , Iterator
    , foldTree
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( liftM )

import System.Directory ( Permissions(searchable), getDirectoryContents, getPermissions )
import System.FilePath ( (</>) )

-------------------------------------------------------------------

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

-------------------------------------------------------------------

data ItResult = Done | Skip | Continue
                deriving (Show)

type Iterator a = a -> FilePath -> IO (ItResult, a)

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = snd `liftM` fold initSeed path
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed

    walk seed (name:names) = do
      let path' = path </> name
      perms <- getPermissions path'
      rs@(r, seed') <- iter seed path'
      case r of
        Done -> return rs
        Skip -> walk seed' names
        Continue
          | searchable perms -> do
              rs'@(r', seed'') <- fold seed' path'
              case r' of
                Done -> return rs'
                _    -> walk seed'' names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue, seed)
