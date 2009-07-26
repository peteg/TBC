module T where
import Sample

-- bad test, assumes that Stack has an Ord instance
test_ord = push (1::Int) empty > empty

test_trivial = empty == empty