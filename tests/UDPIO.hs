module Main where

import Network.TFTP.UDPIO
import Network.TFTP.Types

import System.IO(stdout)

import Control.Concurrent
import Control.Concurrent.MVar

main = do
  init_logging

  let toClient = toBS "hello client!"
      toServer = toBS "hello server!"

  serverV <- newEmptyMVar

  fromClientV <- newEmptyMVar
  forkIO (udpIO Nothing (Just "1234") (server toClient serverV fromClientV))

  fromServerV <- newEmptyMVar
  forkIO (udpIO (Just "localhost") Nothing (client toServer serverV fromServerV))

  fromServer <- takeMVar fromServerV
  fromClient <- takeMVar fromClientV

  when (fromServer /= toClient) (error "fromServer /= toClient")
  when (fromClient /= toServer) (error "fromServer /= toClient")

server :: ByteString -> MVar Address -> MVar ByteString -> UDPIO ()
server toClient serverV fromClientV = do
  me <- localAddress
  liftIO (putMVar serverV me)
  Just (clientAddr, fromClient) <- receiveFrom Nothing
  liftIO (putMVar fromClientV fromClient)
  sendTo clientAddr toClient

client :: ByteString -> MVar Address -> MVar ByteString -> UDPIO ()
client toServer serverV fromServerV = do
  serverAddr <- liftIO (takeMVar serverV)
  sendTo serverAddr toServer
  Just (_, fromServer) <- receiveFrom Nothing
  liftIO $ putMVar fromServerV fromServer

toBS = bpack . (map (fromIntegral . fromEnum))

init_logging = do
  h <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName
                         ( setLevel DEBUG . addHandler h)
