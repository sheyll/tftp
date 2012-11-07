module Main where

import Network.TFTP.Protocol
import Network.TFTP.UDPIO
import Network.TFTP.Types

import Prelude hiding (readFile)

import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit

main = do
  args <- getArgs
  case args of
    (fname: port: timeoutArg: verbose) -> do
      case verbose of
        [] -> init_logging WARNING
        ["-v"] -> init_logging INFO
        ["-vv"] -> init_logging DEBUG
      let host = Nothing
          timeout = case read timeoutArg of
            0 -> Nothing
            secs -> Just secs
      fileExists <- doesFileExist fname
      when (not fileExists) (do
        errorM "TFTPUpload" ("File " ++ fname ++ " not found!")
        exitWith (ExitFailure 2))
      serverFile fname host (Just port) timeout

    _ -> do
      errorM "TFTPUpload" "Missing parameter. Expected: <filename> <port> <timeout> [-v|-vv]"
      exitWith (ExitFailure 1)


serverFile fname host port timeout = do
  cont <- readFile fname
  infoM "TFTPUpload" (printf "Serving file '%s' under the name 'xxx'" fname)
  udpIO host port (runTFTP (offerSingleFile timeout "xxx" cont))

init_logging level = do
  updateGlobalLogger rootLoggerName $ setLevel level
