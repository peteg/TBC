module T where

import Tests.Basis

prop_push_pop_idempotent = property (\s a -> s == pop (push (a::Int) s))

-- dodgy test
prop_pop_push_mostly_idempotent =
    property (\s -> let x = top s in s == push (x::Int) (pop s))

-- fixed
prop_pop_push_idempotent =
    property (\s -> let x = top s in s == empty || s == push (x::Int) (pop s))

-- this doesn't work as 'Stack's are not in the 'NFData' class.
-- exception_pop_empty = pop empty

-- so we say something less pleasant.
exception_pop_empty = top (pop empty)
