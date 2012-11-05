-- | Buffered UDP IO utility module.
module Network.TFTP.UDPIO ( UDPIO(..)
                          , Address
                          , udpIO) where

import qualified Network.Socket as Sock

import Network.TFTP.Types

import Foreign.Ptr(Ptr(..))

import Foreign.Marshal( mallocArray
                      , peekArray
                      , pokeArray
                      , free)

-- | Network address of a UDP sender/receiver
type Address = Sock.SockAddr

-- | Internal state
data UDPIOSt = UDPIOSt { udpReader :: Reader
                       , udpWriter :: Writer
                       , udpMyAddress :: Address
                       }

-- | A monad for UDP IO
newtype UDPIO a = UDPIO { runUDPIO :: StateT UDPIOSt IO a }
    deriving (Functor, Monad, MonadIO, MonadState UDPIOSt, Applicative)

-- | Abstraction over UDP IO for sending/receiving bytestrings
instance MessageIO UDPIO Address ByteString where
    sendTo to msg = do
      w <- udpWriter <$> get
      liftIO (w to msg)

    receiveFrom timeout = do
      r <- udpReader <$> get
      liftIO r

    localAddress = udpMyAddress <$> get

-- | Execute an action on a bound UDP port providing access to UDP IO via
-- two functions that read and write data to/from UDP sockets.
-- When the action returns, the socket is closed.
udpIO :: Maybe String
          -- ^ Hostname where the local UDP port will be bound
          -> Maybe String
          -- ^ Port where the local UDP port will be bound
          -> UDPIO a
          -- ^ The action to run with a reader and a writer
          -> IO a
          -- ^ Result of the action.
udpIO host port action =
    do
      (addr, sock) <- bindUDPSocket host port
      readBuf <- mallocArray bufferSize
      let reader = makeReader sock readBuf
      writeBuf <- mallocArray bufferSize
      let writer = makeWriter sock writeBuf
      res <- evalStateT (runUDPIO action) (UDPIOSt reader writer addr)
      free readBuf
      free writeBuf
      Sock.sClose sock
      return res

bufferSize = 4096

-- | Create a socket bound to some address. One of hostname or port MUST be specified.
bindUDPSocket :: Maybe String -> Maybe String -> IO (Address, Sock.Socket)
bindUDPSocket hostname port = do
  let myHints = Sock.defaultHints { Sock.addrFlags = [Sock.AI_PASSIVE] }
  (serverAddr:_) <- Sock.getAddrInfo (Just myHints) hostname port
  sock <- Sock.socket (Sock.addrFamily serverAddr) Sock.Datagram Sock.defaultProtocol
  let addr = Sock.addrAddress serverAddr
  Sock.bindSocket sock addr
  boundAddr <- Sock.getSocketName sock
  logInfo $ printf "Bound socket at address %s" (show boundAddr)
  return (boundAddr, sock)

makeReader ::  Sock.Socket -> Ptr Word8 -> Reader
makeReader sock buffer = do
            (bytesRead, from) <- Sock.recvBufFrom sock buffer bufferSize
            logInfo $ "Read " ++ show bytesRead ++ " bytes from " ++ (show from)
            res <- peekArray bytesRead buffer
            return (from, bpack res)

makeWriter :: Sock.Socket -> Ptr Word8 -> Writer
makeWriter sock buffer destAddr toSend = writeLoop $ bunpack toSend
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

-- | The type of the action that reads a UDP packet coming together with its
-- origination
type Reader = IO (Address, ByteString)

-- | The type of functions that write a bytestring to an address.
type Writer = Address -> ByteString -> IO ()
