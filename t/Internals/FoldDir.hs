
import Test.HUnit

import Test.TBC.FoldDir

it1 :: Int -> Iterator Int
it1 n x _f
    | x == n = return (Done, x)
    | x > n  = error $ "Iteration has gone too far: visited " ++ show x ++ " files."
    | otherwise = return (Continue, x + 1)

-- FIXME UNIXism.
test_FoldDir_1 :: Assertion
test_FoldDir_1 =
   do i <- foldTree (it1 n) 0 "/usr/bin"
      assertEqual "FoldDir visits exactly the right number of files" n i
  where
    n = 10
