{- Test By Convention: executable top-level.
 - Copyright   :  (C)opyright 2009-2012 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Main ( main ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( unless, when )
import Data.List ( foldl', isSuffixOf )

import Distribution.PackageDescription ( GenericPackageDescription )
import Distribution.PackageDescription.Parse ( readPackageDescription )

import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program ( builtinPrograms, restoreProgramConfiguration )
import Distribution.Simple.Setup
  ( ConfigFlags(..), Flag(..), configPrograms, configVerbosity, defaultDistPref, fromFlag )
import Distribution.Simple.Utils ( defaultPackageDesc )

import System.Directory ( getCurrentDirectory, getDirectoryContents, setCurrentDirectory )
import System.Exit -- ( ExitFailure, exitWith )
import System.Environment ( getArgs, getProgName )
import System.FilePath -- ( takeDirectory ) -- FIXME

import qualified System.Console.GetOpt as GetOpt

import Test.TBC ( Verbosity, deafening, debug, info, normal, notice
                , tbcCabal )

-------------------------------------------------------------------
-- TBC-as-an-executable.
-------------------------------------------------------------------

-- | Find a @.cabal@ file that might apply to the current
-- directory. FIXME robustness? Windows? Efficiency?
-- Also want to dodge the ~/.cabal directory.
-- FIXME repair relPath
findCabal :: IO (Maybe (FilePath, FilePath, FilePath))
findCabal = getCurrentDirectory >>= searchUp ["."] . splitPath
  where
    searchUp :: [FilePath] -> [FilePath] -> IO (Maybe (FilePath, FilePath, FilePath))
    searchUp relPath path =
      do fs <- getDirectoryContents curdir
         case filter (".cabal" `isSuffixOf`) fs of
           [] -> case nextPath of
                   [] -> return Nothing
                   _  -> searchUp (last path : relPath) nextPath
           [cabal] -> return (Just (joinPath relPath, curdir, cabal))
           fs' -> error $ "FIXME: several cabal files found: " ++ show fs'
      where
        curdir = joinPath path
        nextPath = init path

----------------------------------------

-- | Program arguments.
data Options =
    Options
    { optVerbosity   :: Verbosity
    } deriving Show

defaultOptions :: Options
defaultOptions =
    Options
    { optVerbosity   = normal
    }

-- | FIXME use intToVerbosity
options :: [GetOpt.OptDescr (Options -> Options)]
options =
    [ GetOpt.Option ['v']     ["verbose"]
        (GetOpt.NoArg (\ opts -> opts { optVerbosity = deafening })) -- verbose
        "chatty output on stdout"
    ]

progOpts :: [String] -> IO (Options, [String])
progOpts argv =
  case GetOpt.getOpt GetOpt.Permute options argv of
      (o, n, []  ) -> return (foldl' (flip id) defaultOptions o, n)
      (_, _, errs) ->
        do progName <- getProgName
           ioError (userError (concat errs ++ GetOpt.usageInfo (header progName) options))
  where header progName = "Usage: " ++ progName ++ " [OPTION...] files..."

----------------------------------------

-- | FIXME infinite room for improvement.
-- FIXME Make use of the options
main :: IO ()
main =
 do (opts, tests) <- progOpts =<< getArgs

    debug (optVerbosity opts) $ "Options: " ++ show opts
    unless (null tests) $ putStrLn $ "Testing: " ++ show tests

    cabalLoc <- findCabal
    case cabalLoc of
      Nothing ->
        do putStrLn ".cabal file not found."
           exitWith (ExitFailure 1)
      Just (testPath, root, cabalFile) ->
        do
           info (optVerbosity opts) $
             "Running tests with Cabal file: '" ++ cabalFile ++ "' in directory: " ++ testPath
           -- Change to the project root directory
           setCurrentDirectory root
           when (optVerbosity opts >= deafening) $
             getCurrentDirectory >>= \s -> putStrLn $ "In directory: " ++ s

           -- FIXME assume the dist/ dir is with the .cabal file.
           -- No good evidence for this except it's the Simple thing to do.
           let distPref = defaultDistPref
               verbosity = optVerbosity opts -- fromFlag $ buildVerbosity flags
           localbuildinfo <- getBuildConfig hooks verbosity distPref
           let pkg_descr = localPkgDescr localbuildinfo

           let ts = case tests of
                      [] -> [ testPath ]
                      _  -> [ testPath </> t | t <- tests ]

           tbcCabal (optVerbosity opts) ts False pkg_descr localbuildinfo
  where hooks = simpleUserHooks -- error "FIXME Simple only for now."

----------------------------------------

-- Ripped from Cabal (Distribution.Simple). *sigh* Why not export more stuff?

-- | FIXME we assume the user isn't doing anything clever with
-- UserHooks. This info lies in Setup.hs, a bit beyond our reach.
getBuildConfig :: UserHooks -> Verbosity -> FilePath -> IO LocalBuildInfo
getBuildConfig hooks verbosity distPref = do
  lbi_wo_programs <- getPersistBuildConfig distPref
  -- Restore info about unconfigured programs, since it is not serialized
  let lbi = lbi_wo_programs {
    withPrograms = restoreProgramConfiguration
                     (builtinPrograms ++ hookedPrograms hooks)
                     (withPrograms lbi_wo_programs)
  }

  case pkgDescrFile lbi of
    Nothing -> return lbi
    Just pkg_descr_file -> do
      outdated <- checkPersistBuildConfigOutdated distPref pkg_descr_file
      if outdated
        then reconfigure pkg_descr_file lbi
        else return lbi

  where
    reconfigure :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
    reconfigure pkg_descr_file lbi = do
      notice verbosity $ pkg_descr_file ++ " has been changed. "
                      ++ "Re-configuring with most recently used options. " 
                      ++ "If this fails, please run configure manually.\n"
      let cFlags = configFlags lbi
      let cFlags' = cFlags {
            -- Since the list of unconfigured programs is not serialized,
            -- restore it to the same value as normally used at the beginning
            -- of a conigure run:
            configPrograms = restoreProgramConfiguration
                               (builtinPrograms ++ hookedPrograms hooks)
                               (configPrograms cFlags),

            -- Use the current, not saved verbosity level:
            configVerbosity = Flag verbosity
          }
      configureAction hooks cFlags' (extraConfigArgs lbi)

configureAction :: UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction hooks flags args = do
                let distPref = fromFlag $ configDistPref flags
                pbi <- preConf hooks args flags

                (mb_pd_file, pkg_descr0) <- confPkgDescr

                --    get_pkg_descr (configVerbosity flags')
                --let pkg_descr = updatePackageDescription pbi pkg_descr0
                let epkg_descr = (pkg_descr0, pbi)

                --(warns, ers) <- sanityCheckPackage pkg_descr
                --errorOut (configVerbosity flags') warns ers

                localbuildinfo0 <- confHook hooks epkg_descr flags

                -- remember the .cabal filename if we know it
                -- and all the extra command line args
                let localbuildinfo = localbuildinfo0 {
                                       pkgDescrFile = mb_pd_file,
                                       extraConfigArgs = args
                                     }
                writePersistBuildConfig distPref localbuildinfo

                let pkg_descr = localPkgDescr localbuildinfo
                postConf hooks args flags pkg_descr localbuildinfo
                return localbuildinfo
              where
                verbosity = fromFlag (configVerbosity flags)
                confPkgDescr :: IO (Maybe FilePath, GenericPackageDescription)
                confPkgDescr = do
                  mdescr <- readDesc hooks
                  case mdescr of
                    Just descr -> return (Nothing, descr)
                    Nothing -> do
                      pdfile <- defaultPackageDesc verbosity
                      descr  <- readPackageDescription verbosity pdfile
                      return (Just pdfile, descr)
