module Main where

import Network.TFTP.UDPIO
import Network.TFTP.Message

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO(stdout)

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad

main = do
  init_logging

  let toClient = toBS "hello client!"
      toServer = toBS "hello server!"

  serverV <- newEmptyMVar

  fromClientV <- newEmptyMVar
  forkIO (withUDPIO Nothing (Just "1234") (server toClient serverV fromClientV))

  fromServerV <- newEmptyMVar
  forkIO (withUDPIO (Just "localhost") Nothing (client toServer serverV fromServerV))

  fromServer <- takeMVar fromServerV
  fromClient <- takeMVar fromClientV

  when (fromServer /= toClient) (error "fromServer /= toClient")
  when (fromClient /= toServer) (error "fromServer /= toClient")

server :: ByteString -> MVar Address -> MVar ByteString -> UDPIOAction ()
server toClient serverV fromClientV myAddr reader writer = do
  putMVar serverV myAddr
  (clientAddr, fromClient) <- reader
  putMVar fromClientV fromClient
  writer clientAddr toClient

client :: ByteString -> MVar Address -> MVar ByteString -> UDPIOAction ()
client toServer serverV fromServerV myAddr reader writer = do
  serverAddr <- takeMVar serverV
  writer serverAddr toServer
  (_, fromServer) <- reader
  putMVar fromServerV fromServer

toBS = pack . (map (fromIntegral . fromEnum))

init_logging = do
  h <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName
                         ( setLevel DEBUG
                           . addHandler h)
