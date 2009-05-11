{- Test By Convention: A variant of FoldDir from Real World Haskell.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
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
getUsefulContents path =
    filter (`notElem` [".", ".."]) `liftM` getDirectoryContents path

-------------------------------------------------------------------

data ItResult = Continue | Done
                deriving (Show)

type Iterator a = a -> FilePath -> IO (ItResult, a)

-- | Visit all files in a directory tree. Note we don't invoke the
-- iterator on directories, only on files.
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = snd `liftM` fold initSeed path
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath

    walk seed _ [] = return (Continue, seed)
    walk seed subpath (name:names) =
      do let path' = subpath </> name
         perms <- getPermissions path'
         rs@(r, seed') <- if searchable perms
                            then fold seed path' -- It's a directory, Jim.
                            else iter seed path' -- It's a file.
         case r of
           Continue -> walk seed' subpath names
           Done     -> return rs
