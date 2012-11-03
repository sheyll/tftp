module Network.TFTP.Server where

import qualified Network.Socket as Sock
import System.IO(hSetBuffering, hSetBinaryMode, hWaitForInput, hClose, hGetBufSome, hPutBuf, hFlush, BufferMode(..), Handle, IOMode(..))
import Text.Printf(printf)
import Foreign.Ptr(Ptr(..))
import Foreign.Marshal(mallocArray, peekArray, pokeArray, free)
import Data.Word(Word8)
import Control.Monad(join)
import Network.TFTP.Message(Message, encode, decode, ByteString, pack, unpack)

bufferSize = 4096
tftpPort = 69

serveFiles serverHost serverPort dir timeout =
    withTFTPClient serverHost serverPort timeout (tftpFileServer dir)

bindUDPSocket hostname port = do
  (serverAddr:_) <- Sock.getAddrInfo Nothing (Just hostname) (Just port)
  sock <- Sock.socket (Sock.addrFamily serverAddr) Sock.Datagram Sock.defaultProtocol
  Sock.bind sock (Sock.addrAddress serverAddr)
  return sock

tftpFileServer :: FilePath -> Server ()
tftpFileServer dir reader writer = do
  (from, req) <- reader
  printf "got: %s\n" (show req)
  printf "got: %s\n" (show (decode req :: Message))
  writer from req
  tftpFileServer dir reader writer

withTFTPClient :: String -> Int -> Int -> Server () -> IO ()
withTFTPClient host port timeout action =
    Sock.withSocketsDo $ do
      sock <- bindUDPSocket host (show port)
      readBuf <- mallocArray bufferSize
      let reader = makeReader sock readBuf
      writeBuf <- mallocArray bufferSize
      let writer = makeWriter sock writeBuf
      res <- action reader writer
      free readBuf
      free writeBuf
      Sock.close sock

type Server a = Reader -> Writer -> IO a

type Reader = IO (Sock.SockAddr, ByteString)
type Writer = Sock.SockAddr -> ByteString -> IO ()

makeReader :: Sock.Socket -> Ptr Word8 -> Reader
makeReader sock buffer = do
            (bytesRead, from) <- Sock.recvBufFrom sock buffer bufferSize
            putStrLn $ "Read " ++ show bytesRead ++ " bytes from " ++ (show from)
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
        putStrLn $ "Sent " ++ show bytesSent ++ " " ++ show (length rest) ++ " left"
        writeLoop rest