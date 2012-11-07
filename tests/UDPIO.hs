module Main where

import Network.TFTP.UDPIO
import Network.TFTP.Types

import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket(SockAddr(..))
import System.Exit

import System.IO(stdout)

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

  testSendToFails

testSendToFails = udpIO (Just "localhost") Nothing $ do
  res <- sendTo (SockAddrInet 123 0xffffffff) (toBS "hello client")
  assertEqual "testSendToFails" False res

server :: ByteString -> MVar Address -> MVar ByteString -> UDPIO ()
server toClient serverV fromClientV = do
  me <- localAddress
  liftIO (putMVar serverV me)
  Just (clientAddr, fromClient) <- receiveFrom Nothing
  liftIO (putMVar fromClientV fromClient)
  sendTo clientAddr toClient
  return ()

client :: ByteString -> MVar Address -> MVar ByteString -> UDPIO ()
client toServer serverV fromServerV = do
  serverAddr <- liftIO (takeMVar serverV)
  res <- sendTo serverAddr toServer
  assertEqual "client - sendTo" True res
  Just (_, fromServer) <- receiveFrom Nothing
  liftIO $ putMVar fromServerV fromServer

toBS = bpack . (map (fromIntegral . fromEnum))

init_logging = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

assertEqual msg a b = do
  let errMsg = printf "assertEqual FAIL: \"%s\"\nexpected: \"%s\"\n\nactual: \"%s\"\n\n"
                msg (show a) (show b)
  when (a /= b) (testFail errMsg)
  return ()

testFail msg = liftIO $ (putStrLn msg >>  exitWith (ExitFailure 1))

l m = liftIO $ infoM "Test" m
