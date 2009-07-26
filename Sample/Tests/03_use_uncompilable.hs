module T where

import Uncompilable

prop_push_pop_idempotent = property (\s a -> (a,s) == top_pop (push (a::Int) s))