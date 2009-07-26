{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sample
    (
      Stack

    , empty

    , pop
    , push
    , top

    ) where

import Test.QuickCheck

newtype Stack a = Stack { unStack :: [a] }
  deriving (Arbitrary, Eq, Show)

pop :: Stack a -> Stack a
pop = Stack . tail . unStack

push :: a -> Stack a -> Stack a
push a = Stack . (a:) . unStack

empty :: Stack a
empty = Stack []

top :: Stack a -> a
top = head . unStack 
