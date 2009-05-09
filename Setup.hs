import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks hooks
    where hooks = simpleUserHooks { runTests = tbcTests }

tbcTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tbcTests = error "FIXME tbcTests"
