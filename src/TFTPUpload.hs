module Main where

import Network.TFTP.Server
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
      res <- singleBinary timeout fname "xxx" host (Just port)
      case res of
        Nothing ->
          printf "Transfer successful.\n\n"
        Just err -> do
          printf "Transfer failed: %s.\n\n" err
          exitWith (ExitFailure 2)

    _ -> do
      errorM "TFTPUpload" "Missing parameter. Expected: <filename> <port> <timeout> [-v|-vv]"
      exitWith (ExitFailure 1)

init_logging level = do
  updateGlobalLogger rootLoggerName $ setLevel level
