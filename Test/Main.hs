{- Test By Convention: executable top-level.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Main ( main ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Monad ( unless )
import Data.List ( foldl', isSuffixOf )

import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.UserHooks ( UserHooks, emptyUserHooks )
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program ( builtinPrograms, restoreProgramConfiguration )
import Distribution.Simple.Setup ( defaultDistPref )
import Distribution.Verbosity ( Verbosity, normal, verbose )

import System.Directory ( getCurrentDirectory, getDirectoryContents, setCurrentDirectory )
import System.Exit -- ( ExitFailure, exitWith )
import System.Environment ( getArgs, getProgName )
import System.FilePath -- ( takeDirectory ) -- FIXME

import qualified System.Console.GetOpt as GetOpt

import Test.TBC ( tbcCabal )

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
    , optShowVersion :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions =
    Options
    { optVerbosity   = normal
    , optShowVersion = False
    }

-- | FIXME use intToVerbosity
options :: [GetOpt.OptDescr (Options -> Options)]
options =
    [ GetOpt.Option ['v']     ["verbose"]
        (GetOpt.NoArg (\ opts -> opts { optVerbosity = verbose }))
        "chatty output on stdout"
    , GetOpt.Option ['V','?'] ["version"]
        (GetOpt.NoArg (\ opts -> opts { optShowVersion = True }))
        "show version number"
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
-- FIXME Be careful if we're in the project dir and "Tests/" exists. ???
main :: IO ()
main =
 do (opts, tests) <- progOpts =<< getArgs

    unless (null tests) $ putStrLn $ "Testing: " ++ show tests
    putStrLn $ "Options: " ++ show opts

    cabalLoc <- findCabal
    case cabalLoc of
      Nothing ->
        do putStrLn ".cabal file not found."
           exitWith (ExitFailure 1)
      Just (testPath, root, cabalFile) ->
        do
           putStrLn $ "Running tests with Cabal file: '" ++ cabalFile ++ "' in directory: " ++ testPath
           -- Change to the project root directory
           setCurrentDirectory root
           -- getCurrentDirectory >>= \s -> putStrLn $ "In directory: " ++ s

           -- FIXME assume the dist/ dir is with the .cabal file.
           -- No good evidence for this except it's the Simple thing to do.
           let distPref = defaultDistPref
           localbuildinfo <- getBuildConfig hooks distPref
           let pkg_descr = localPkgDescr localbuildinfo

           let ts = case tests of
                      [] -> [ testPath ]
                      _  -> [ testPath </> t | t <- tests ]

           tbcCabal (optVerbosity opts) ts False pkg_descr localbuildinfo
  where hooks = error "FIXME Simple only for now."

----------------------------------------

-- Stuff ripped from Cabal. *sigh* Why not export more stuff?

-- | FIXME we assume the user isn't doing anything clever with
-- UserHooks. This info lies in Setup.hs, a bit beyond our reach.
getBuildConfig :: UserHooks -> FilePath -> IO LocalBuildInfo
getBuildConfig _hooks distPref = do
  lbi <- getPersistBuildConfig distPref
  case pkgDescrFile lbi of
    Nothing -> return ()
    Just pkg_descr_file -> checkPersistBuildConfig distPref pkg_descr_file
  return lbi {
    withPrograms = restoreProgramConfiguration
                     (builtinPrograms ++ hookedPrograms hooks)
                     (withPrograms lbi)
  }
 where hooks = emptyUserHooks
