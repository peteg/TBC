{- Test By Convention: Drivers.
 - Copyright   :  (C)opyright 2009 {mwotton, peteg42} at gmail dot com
 - License     :  BSD3
 -}
module Test.TBC.Drivers
    ( Driver(..)
    , ghci
    ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------

import Control.Concurrent -- ( forkIO )
import Control.Monad ( liftM )

import Data.List ( isInfixOf )

import Distribution.Simple.Utils ( info, debug )
import Distribution.Verbosity ( Verbosity )

import System.Exit
import System.IO -- ( hClose, hFlush, hGetContents, hPutStr )
import System.Posix.Signals ( installHandler, sigINT, Handler(..) )
import System.Process ( runInteractiveProcess, waitForProcess, terminateProcess )

-------------------------------------------------------------------

-- Drivers
data Driver
    = MkDriver
      { hci_send_cmd :: String -> IO [String] -- ^ FIXME exec and return lines of response.
      , hci_load_file :: String -> IO [String] -- ^ FIXME exec and return lines of response.
      , hci_close :: IO ExitCode
      }

----------------------------------------

-- | GHCi driver, slave process.
ghci :: Verbosity
     -> String -- ^ ghci command name
     -> [String] -- ^ flags
     -> IO Driver
ghci verbosity cmd flags =
  do debug verbosity $
       unlines [ "system $ " ++ cmd ++ " " ++ concat [ ' ' : a | a <- flags ]
               , "----------------------------------------" ]
     (hin, hout, herr, hpid)
         <- runInteractiveProcess cmd flags Nothing Nothing -- FIXME

     -- TODO arguably other signals too
     -- TODO timeouts: although perhaps bad idea to arbitrarily limit time for a test run
     -- TODO windows: now we need to import unix package for System.Posix.Signals
     installHandler sigINT (Catch $ do
        terminateProcess hpid
        exitFailure
      ) Nothing

     -- We don't use GHCi's stderr, get rid of it.
     -- FIXME we maybe have to drain it first.
     hClose herr

     let load_file f =
           do cout <- ghci_sync verbosity hin hout (":l " ++ f ++ "\n")
              if not (null cout) && "Ok, modules loaded" `isInfixOf` last cout
                then return []
                else return cout

     return $ MkDriver
                { hci_send_cmd = ghci_sync verbosity hin hout
                , hci_load_file = load_file
                , hci_close = do hPutStr hin ":quit\n"
                                 hFlush hin
                                 waitForProcess hpid
                }

ghci_sync :: Verbosity
          -> Handle -> Handle -> String -> IO [String]
ghci_sync verbosity hin hout inp =
  do info verbosity $
          "--Sync----------------------------------\n"
       ++ inp
       ++ "----------------------------------------\n"

     -- FIXME do we really need the separate thread?
     -- Get output + sync
     outMVar <- newEmptyMVar
     forkIO $ hc_sync hout >>= putMVar outMVar

     -- Tests + sync
     hPutStr hin inp
     hPutStr hin hc_sync_print
     hFlush hin

     -- Synchronize
     hc_output <- lint_output `liftM` takeMVar outMVar

     info verbosity $
       unlines ( ">> Output <<" : hc_output )

     return hc_output
  where
    lint_output :: [[a]] -> [[a]]
    lint_output = reverse . dropWhile null . reverse . dropWhile null

    done :: String
    done = ">>>>done<<<<"

    hc_sync_print :: String
    hc_sync_print = "System.IO.putStrLn \"" ++ done ++ "\"\n"

    -- FIXME EOF, exceptions, etc.
    hc_sync :: Handle -> IO [String]
    hc_sync h = sync
        where
          sync =
              do l <- hGetLine h
--                  putStrLn $ "hc>> " ++ l
                 if done `isInfixOf` l
                   then return []
                   else (l:) `liftM` sync
