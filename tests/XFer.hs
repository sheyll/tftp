module Main where

import System.Log.Logger
import Network.TFTP.XFer

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO(stdout)


main = do
  init_logging


init_logging = do
  h <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName
                         ( setLevel DEBUG
                           . addHandler h)
