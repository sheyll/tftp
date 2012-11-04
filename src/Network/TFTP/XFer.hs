-- | Transmission of data via TFTP. This implements the stop-and-wait style data
-- transmission protocol.
module Network.TFTP.XFer where

import qualified Network.TFTP.UDPIO as UDP
import qualified Network.TFTP.Message as M

import qualified Data.ByteString.Lazy as B

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Applicative

import System.Log.Logger
import Data.Word
import Text.Printf

maxRetries :: Int
maxRetries = 3

timeout :: Int
timeout = 3

type XFerM = StateT XFerState IO

write :: B.ByteString -> XFerM ()
write block = write maxRetries block
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
            sendData (B.take 512 blk)
            mAckIdx <- receiveAck
            case mAckIdx of
              Just ackIdx | ackIdx == blockIdx ->
                  do printInfo $ printf "Writing block #%s acknowledged." blockIdx
                     if B.length blk >= 512 then
                         do incBlockIndex
                            write maxRetries (B.drop 512 blk)
                     else
                         printInfo $ printf "Write finished"

              Nothing ->
                  do printWarn $ printf "Timeout writing block #%s, ACK not received withing: %i seconds" blockIdx timeout
                     write (retries - 1) blk

              Just otherIdx ->
                  do printWarn $ printf "Failed to write block #%s, receive ACK for block #%i" blockIdx otherIdx
                     write (retries - 1) blk

data XFerState = XFerState { xsBlockIndex :: Word16
                           , xsWriter :: UDP.Writer
                           , xsReader :: UDP.Reader
                           }

getBlockIndex = xsBlockIndex <$> get
sendData = undefined
receiveAck = undefined
incBlockIndex = undefined
sendError = undefined

printInfo m = liftIO $ infoM "TFTP.XFer" m
printWarn m = liftIO $ warningM "TFTP.XFer" m
printErr  m = liftIO $ errorM "TFTP.XFer" m
