import Distribution.Simple
import Test.TBC

main = defaultMainWithHooks hooks
    where hooks = simpleUserHooks { runTests = tbcCabal }
