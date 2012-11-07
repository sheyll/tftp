-- | Transmission of data via TFTP. This implements the stop-and-wait style data
-- transmission protocol.
module Network.TFTP.Protocol where

import           Network.TFTP.Types

import qualified Network.TFTP.UDPIO as UDP
import qualified Network.TFTP.Message as M

import           System.Log.Logger
import           Data.Word
import           Text.Printf

import           Control.Monad.Trans.Maybe

-- | XFer monad parameterised over a (MessageIO) monad.
type XFerT m address a = StateT (XFerState address) m a

-- | Execute a transfer action.
runTFTP :: (MessageIO m address) => XFerT m address result -> m result
runTFTP action = evalStateT action (XFerState 0 Nothing)

-- | A simple server action that will wait for a RRQ for its file.
offerSingleFile :: (MessageIO m address) => Maybe Int -> String -> ByteString -> XFerT m address Bool
offerSingleFile timeoutSeconds fname content = do
  req <- receive timeoutSeconds
  case req of
    Just (M.RRQ rfname mode) | rfname == fname  -> do
      when (mode == M.NetASCII)
        (printWarn "Warning: a client requested NetASCII sending Octet data instead!")
      printInfo $ printf "Accepting RRQ for %s sending %i bytes!" fname (blength content)
      resetBlockIndex
      incBlockIndex
      writeData content

    Just (M.RRQ rfname _) -> do
      printErr $ printf "Client request for %s but I can send only %s" rfname fname
      reply $ M.Error $ M.FileNotFound
      return False

    Just req -> do
      printErr $ printf "Invalid client request %s" (show req)
      reply $ M.Error $ M.IllegalTFTPOperation
      return False

    Nothing -> do
      printErr $ printf "Timeout offering single file '%s'" fname
      return False

-- | A transfer action that sends a large chunk of data via TFTP DATA messages
-- to a destination.
writeData :: (MessageIO m address) => ByteString -> XFerT m address Bool
writeData block = write maxRetries block
    where
      write retries blk = do
        replyData (btake 512 blk)
        continueAfterACK (writeNext blk) (retryThisWrite retries blk) writeFailed

      writeNext blk = do
        if blength blk >= 512 then do
                                incBlockIndex
                                write maxRetries (bdrop 512 blk)
         else do
          printInfo $ printf "Write finished"
          return True

      retryThisWrite retries blk =
          if retries == 0 then writeFailed else
              do printWarn $ printf "Retrying..."
                 write (retries - 1) blk

      writeFailed = do
        blockIdx <- getBlockIndex
        printErr $ printf "Write failed after %i retransmissions" maxRetries
        reply $ M.Error $ M.ErrorMessage "timeout"
        return False


-- | Receive the next message from the client, if the client anserws with the
-- correct ack call 'success'. If there was a timeout or the ack was for an
-- invalid index call 'retry', if an error occured call 'error
continueAfterACK success retry fail = do
  currentIdx <- getBlockIndex
  packet <- receive ackTimeOut
  case packet of
    Just (M.Error err) ->
        do printErr $ printf "Error message received:  (%s)  "  (show err)
           fail

    Just (M.ACK idx) | idx == currentIdx ->
       do printInfo $ printf "Acknowledged"
          success

    Just (M.ACK idx) | idx /= currentIdx ->
       do printWarn $ printf "ACK invalid"
          retry

    Just otherMsg ->
       do printErr $ printf "Unexpected message"
          fail

    -- this indicates a timeout
    Nothing -> retry

maxRetries :: Int
maxRetries = 30

ackTimeOut = Just 3

data XFerState address = XFerState { xsBlockIndex  :: Word16
                                   , xsFrom        :: Maybe address}

resetBlockIndex :: Monad m => XFerT m address ()
resetBlockIndex = modify (\st -> st {xsBlockIndex = 0})

getBlockIndex :: Monad m => XFerT m address Word16
getBlockIndex = get >>= return . xsBlockIndex

incBlockIndex :: Monad m => XFerT m address Word16
incBlockIndex = do
   st <- get
   let i = xsBlockIndex st
       i' = i + 1
   modify $ \st -> st { xsBlockIndex = i' }
   return i'

getLastPeer :: Monad m => XFerT m address (Maybe address)
getLastPeer = get >>= return . xsFrom

setLastPeer :: (MessageIO m address) => Maybe address -> XFerT m address ()
setLastPeer addr = do
  lp <- getLastPeer
  when (lp /= addr) $ do
                     printInfo $ printf "Replacing last peer with (%s)" (show addr)
                     modify $ \st -> st { xsFrom = addr }

replyData :: (MessageIO m address) => ByteString -> XFerT m address ()
replyData chunk = do
  idx <- getBlockIndex
  reply (M.DATA idx chunk)

reply :: (MessageIO m address) => M.Message -> XFerT m address ()
reply msg = do
  lp <- getLastPeer
  Just dest <- getLastPeer
  send dest msg

send :: (MessageIO m address) => address -> M.Message -> XFerT m address ()
send dest msg = do
  let msg' = M.encode msg
  lift $ sendTo dest msg'
  printInfo $ printf "Sent message to (%s) (%i bytes)" (show dest) (blength msg')

-- | receive a message and remeber the sender for 'getLastPeer'
receive :: (MessageIO m address) => Maybe Int -> XFerT m address (Maybe M.Message)
receive timeout = do
  res <- lift (receiveFrom timeout)
  case res of
    Just (from, msg) -> do
      setLastPeer (Just from)
      let msg' = M.decode msg
      printInfo (printf "Received msg (%i bytes)" (blength msg))
      return (Just msg')
    Nothing -> do
      printWarn "Receive timeout"
      return Nothing

printInfo :: (MessageIO m address) => String -> XFerT m address ()
printInfo = logWith infoM

printWarn :: (MessageIO m address) => String -> XFerT m address ()
printWarn = logWith warningM

printErr :: (MessageIO m address) => String -> XFerT m address ()
printErr = logWith errorM

logWith :: (MessageIO m address) =>
           (String -> String -> IO ()) -> String -> XFerT m address ()
logWith f m = do
  la <- lift localAddress
  idx <- getBlockIndex
  from <- getLastPeer
  let m' = printf "%s @ block #%i <%s> <%s>" m idx (show la) (show from)
  liftIO (f "TFTP.Protocol" m')
