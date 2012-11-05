-- | Transmission of data via TFTP. This implements the stop-and-wait style data
-- transmission protocol.
module Network.TFTP.XFer where

import Network.TFTP.Types

import qualified Network.TFTP.UDPIO as UDP
import qualified Network.TFTP.Message as M

import System.Log.Logger
import Data.Word
import Text.Printf

-- | XFer monad parameterised over a (MessageIO) monad
type XFerT m address a = StateT (XFerState address) m a

-- | execute a transfer action
runXFerT :: (MonadIO m, Show address, MessageIO m address ByteString)
                         => XFerT m address result -> m result
runXFerT action = evalStateT action (XFerState 0 Nothing)

-- | a transfer action that sends via TFTP to a destination
send block = write maxRetries block
    where
      write 0 _blk =
          do
            blockIdx <- getBlockIndex
            printErr $ printf "No valid ACK for block #%i after %i retries"
                               blockIdx maxRetries
            sendError $ M.ErrorMessage "timeout"

      write retries blk =
          do
            blockIdx <- getBlockIndex
            printInfo $ printf "Writing block #%s." blockIdx
            replyData (btake 512 blk)
            mAckIdx <- receiveAck
            case mAckIdx of
              Just ackIdx | ackIdx == blockIdx ->
                  do printInfo $ printf "Writing block #%s acknowledged." blockIdx
                     if blength blk >= 512 then
                         do incBlockIndex
                            write maxRetries (bdrop 512 blk)
                     else
                         printInfo $ printf "Write finished"

              Nothing ->
                  do printWarn $ printf "Timeout writing block #%s, ACK not received withing: %i seconds" blockIdx timeout
                     write (retries - 1) blk

              Just otherIdx ->
                  do printWarn $ printf "Failed to write block #%s, receive ACK for block #%i" blockIdx otherIdx
                     write (retries - 1) blk

maxRetries :: Int
maxRetries = 3

timeout :: Int
timeout = 3

data XFerState address = XFerState { xsBlockIndex  :: Word16
                                   , xsFrom :: Maybe address}

getBlockIndex :: Monad m => XFerT m address Word16
getBlockIndex = get >>= return . xsBlockIndex

getLastPeer :: Monad m => XFerT m address (Maybe address)
getLastPeer = get >>= return . xsFrom

setLastPeer :: Monad m => Maybe address -> XFerT m address ()
setLastPeer addr = modify $ \st -> st { xsFrom = addr }

replyData :: (MessageIO m address ByteString, MonadIO m, Show address) => ByteString -> XFerT m address ()
replyData chunk = do
  idx <- getBlockIndex
  printInfo $ "Replying data packet"
  reply (M.DATA idx chunk)

reply :: (MessageIO m address ByteString, MonadIO m, Show address) => M.Message -> XFerT m address ()
reply msg = do
  Just dest <- getLastPeer
  let msg' = M.encode msg
  lift $ sendTo dest msg'
  printInfo $ "Replyed a " ++ show (blength msg') ++ " bytes long message."

-- | receive a message and remeber the sender for 'getLastPeer'
receive :: (MessageIO m address ByteString, MonadIO m, Show address) => XFerT m address M.Message
receive = do
  (from, msg) <- lift (receiveFrom 0)
  setLastPeer (Just from)
  printInfo $ "Received a " ++ show (blength msg) ++ " bytes long message."
  return $ M.decode msg

receiveAck = undefined
incBlockIndex = undefined
sendError = undefined

printInfo :: (MessageIO m address ByteString, MonadIO m, Show address) => String -> XFerT m address ()
printInfo = logWith infoM

printWarn :: (MessageIO m address ByteString, MonadIO m, Show address) => String -> XFerT m address ()
printWarn = logWith warningM

printErr :: (MessageIO m address ByteString, MonadIO m, Show address) => String -> XFerT m address ()
printErr = logWith errorM

logWith :: (MessageIO m address ByteString, MonadIO m, Show address) =>
           (String -> String -> IO ()) -> String -> XFerT m address ()
logWith f m = do
  idx <- getBlockIndex
  from <- getLastPeer
  let m' = printf "%s @ XFer [block #%i]  [from %s]" m idx (show from)
  liftIO (f "TFTP.XFer" m')
