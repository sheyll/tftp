module Network.TFTP.Server where

import qualified Network.Socket as Sock
import System.IO(hSetBuffering, hSetBinaryMode, hWaitForInput, hClose, hGetBufSome, hPutBuf, hFlush, BufferMode(..), Handle, IOMode(..))
import Text.Printf(printf)
import Foreign.Ptr(Ptr(..))
import Foreign.Marshal.Array(allocaArray, peekArray, pokeArray)
import Data.Word(Word8)
import Control.Monad(join)

bufferSize = 4096
tftpPort = 69

serveFiles serverHost serverPort dir timeout =
    withTFTPClient serverHost serverPort timeout (tftpFileServer dir)

bindUDPSocket hostname port = do
  (serverAddr:_) <- Sock.getAddrInfo Nothing (Just hostname) (Just port)
  sock <- Sock.socket (Sock.addrFamily serverAddr) Sock.Datagram Sock.defaultProtocol
  Sock.bind sock (Sock.addrAddress serverAddr)
  return sock

tftpFileServer :: FilePath -> Server
tftpFileServer dir reader writer = do
  (from, req) <- reader 10
  writer from req
  printf "echoed %s\n" (show req)
  tftpFileServer dir reader writer

withTFTPClient :: String -> Int -> Int -> Server -> IO ()
withTFTPClient host port timeout action =
    Sock.withSocketsDo $ do
      sock <- bindUDPSocket host (show port)
      allocaArray bufferSize (\readBuf ->
                                  allocaArray bufferSize
                                                  (\writeBuf ->
                                                       action
                                                       (makeReader sock readBuf)
                                                       (makeWriter sock writeBuf)))
      Sock.close sock

type Server = Reader -> Writer -> IO ()

type Reader = Int -> IO (Sock.SockAddr, [Word8])
type Writer = Sock.SockAddr -> [Word8] -> IO ()

makeReader :: Sock.Socket -> Ptr Word8 -> Reader
makeReader sock buffer requestedCnt = readLoop []
    where readLoop acc = do
            let bytesMissing = requestedCnt - (length acc)
                bytesToRead = min bufferSize bytesMissing
            (bytesRead, from) <- Sock.recvBufFrom sock buffer bytesToRead
            putStrLn $ "Read " ++ show bytesRead ++ "/" ++ show requestedCnt ++ " bytes"
            res <- peekArray bytesRead buffer
            let acc' = acc ++ res
            if (length acc') < requestedCnt then
                readLoop acc'
            else
                return (from, acc')

makeWriter :: Sock.Socket -> Ptr Word8 -> Writer
makeWriter sock buffer destAddr toSend = writeLoop toSend
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