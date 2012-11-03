-- | Encapsulates parsing and generation of TFTP Messages
module Network.TFTP.Message
    ( Message(..)
    , Mode(..)
    , module Data.Binary
    , module Data.ByteString.Lazy
    ) where

import Data.Word
import Data.Binary
import Data.Binary.Get(getRemainingLazyByteString)
import Control.Monad
-- import qualified Data.ByteString as B
import Data.ByteString.Lazy(ByteString, pack, unpack)
import Data.Char
import Control.Applicative

data Message = RRQ String Mode |
               WRQ String Mode |
               DATA BlockNumber ByteString |
               ACK BlockNumber |
               Error ErrorNumber ErrorMessage
          deriving(Read, Show, Ord, Eq)

data Mode = NetASCII | Octet
          deriving(Read, Show, Ord, Eq)

type BlockNumber = Word16
type ErrorNumber = Word16
type ErrorMessage = String

newtype NString = NString String

nullTerminated :: String -> NString
nullTerminated = NString

instance Show NString where
    show (NString s) = s

instance Binary NString where
    put (NString str) = forM_ str put >> put ('\NUL':: Char)
    get = get_ []
        where
          get_ acc = do
            c <- get :: Get Char
            if c == '\NUL' then
                return $ NString (reverse acc)
             else
                get_ (c:acc)

data DataChunk = DC ByteString

instance Binary DataChunk where
    put (DC bs) = mapM_ put (unpack bs)

    get = do
      bs <- getRemainingLazyByteString
      return $ DC bs
--      ch <- get_ []
--      return $ DC (pack ch)
--          where get_ acc = isEmpty >>= (\ stop -> if stop then (return (reverse acc)) else (get >>= get_ . (:acc)))

instance Binary Message where
    put (RRQ fname mode) = do
                          put (1 :: Word16)
                          put (nullTerminated fname)
                          put mode

    put (WRQ fname mode) = do
                          put (2 :: Word16)
                          put (nullTerminated fname)
                          put mode

    put (DATA blockIndex chunk) = do
                          put (3 :: Word16)
                          put blockIndex
                          put (DC chunk)

    put (ACK blockIndex) = do
                          put (4 :: Word16)
                          put blockIndex

    put (Error code msg) = do
                          put (5 :: Word16)
                          put code
                          put (nullTerminated msg)

    get = do
      opcode <- get :: Get Word16
      case opcode of
        1 -> do
            NString fname <- get
            mode <- get
            return $ RRQ fname mode

        2 -> do
            NString fname <- get
            mode <- get
            return $ WRQ fname mode

        3 -> do
            blockIndex <- get
            chunk <- get
            return $ DATA blockIndex chunk

        4 -> do
            blockIndex <- get
            return $ ACK blockIndex

        5 -> do
            code <- get
            NString msg <- get
            return $ Error code msg

instance Binary Mode where
    put NetASCII = put $ NString "netascii"
    put Octet = put $ NString "octet"

    get = do
      NString str <- get
      return $ case toLower <$> take (length "octet") str of
                 "netas" -> NetASCII
                 "octet" -> Octet
