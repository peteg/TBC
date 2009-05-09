
import Test.TBC.FoldDir

it1 :: Int -> Iterator Int
it1 n x _f
    | x == n = return (Done ,x)
    | x > n  = error $ "Iteration has gone too far: visited " ++ show x ++ " files."
    | otherwise = return (Continue, x + 1)

-- FIXME UNIXism.
test_FoldDir_1 :: IO Bool
test_FoldDir_1 =
    do i <- foldTree (it1 n) 0 "/usr/bin"
       return $ i == n
  where
    n = 10
