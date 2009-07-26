module Uncompilable where

import Sample

-- intentionally violates abstraction
top_pop :: Stack a -> (a, Stack a)
top_pop (Stack s) = (head s, tail s)
