-- | Buffered UDP IO utility module.
module Network.TFTP.UDPIO ( UDPIOAction
                          , Reader
                          , Writer
                          , withUDPIO
                          , Address
                          ) where

import qualified Network.Socket as Sock

import Network.TFTP.Message( ByteString
                           , pack
                           , unpack)

import System.Log.Logger( infoM
                        , warningM
                        , errorM)

import Text.Printf(printf)

import Foreign.Ptr(Ptr(..))

import Foreign.Marshal( mallocArray
                      , peekArray
                      , pokeArray
                      , free)

import Data.Word( Word8
                , Word16)

-- | Network address of a UDP sender/receiver
type Address = Sock.SockAddr

-- | The type of an action passed to 'withUDPIO'
type UDPIOAction a = Address -> Reader -> Writer -> IO a

-- | The type of the action that reads a UDP packet coming together with its
-- origination
type Reader = IO (Address, ByteString)

-- | The type of functions that write a bytestring to an address.
type Writer = Address -> ByteString -> IO ()

-- | Execute an action on a bound UDP port providing access to UDP IO via
-- two functions that read and write data to/from UDP sockets.
-- When the action returns, the socket is closed.
withUDPIO :: Maybe String
          -- ^ Hostname where the local UDP port will be bound
          -> Maybe String
          -- ^ Port where the local UDP port will be bound
          -> UDPIOAction a
          -- ^ The action to run with a reader and a writer
          -> IO a
          -- ^ Result of the action.
withUDPIO host port action =
    do
      (addr, sock) <- bindUDPSocket host port
      readBuf <- mallocArray bufferSize
      let reader = makeReader sock readBuf
      writeBuf <- mallocArray bufferSize
      let writer = makeWriter sock writeBuf
      res <- action addr reader writer
      free readBuf
      free writeBuf
      Sock.close sock
      return res

bufferSize = 4096

-- | Create a socket bound to some address. One of hostname or port MUST be specified.
bindUDPSocket :: Maybe String -> Maybe String -> IO (Address, Sock.Socket)
bindUDPSocket hostname port = do
  let myHints = Sock.defaultHints { Sock.addrFlags = [Sock.AI_PASSIVE] }
  (serverAddr:_) <- Sock.getAddrInfo (Just myHints) hostname port
  sock <- Sock.socket (Sock.addrFamily serverAddr) Sock.Datagram Sock.defaultProtocol
  let addr = Sock.addrAddress serverAddr
  Sock.bind sock addr
  boundAddr <- Sock.getSocketName sock
  logInfo $ printf "Bound socket at address %s" (show boundAddr)
  return (boundAddr, sock)

makeReader ::  Sock.Socket -> Ptr Word8 -> Reader
makeReader sock buffer = do
            (bytesRead, from) <- Sock.recvBufFrom sock buffer bufferSize
            logInfo $ "Read " ++ show bytesRead ++ " bytes from " ++ (show from)
            res <- peekArray bytesRead buffer
            return (from, pack res)

makeWriter :: Sock.Socket -> Ptr Word8 -> Writer
makeWriter sock buffer destAddr toSend = writeLoop $ unpack toSend
    where
      writeLoop []     = return ()
      writeLoop toSend = do
        let bytesToSend = min bufferSize (length toSend)
            chunkToSend = take bytesToSend toSend
        pokeArray buffer chunkToSend
        bytesSent <- Sock.sendBufTo sock buffer bytesToSend destAddr
        let rest = drop bytesSent toSend
        logInfo $ "Sent " ++ show bytesSent ++ " " ++ show (length rest) ++ " left"
        writeLoop rest

logInfo  = infoM "TFTP.UDPIO"
logWarn  = warningM "TFTP.UDPIO"
logError = errorM "TFTP.UDPIO"
