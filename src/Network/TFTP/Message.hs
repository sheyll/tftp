-- | Encapsulates parsing and generation of TFTP Messages
module Network.TFTP.Message
    ( Message(..)
    , Mode(..)
    , TFTPError(..)
    , decode
    , encode
    , convertMode
    ) where

import Data.Word
import Data.Binary
import Data.Binary.Get(getLazyByteStringNul, getRemainingLazyByteString)
import Control.Monad

import Network.TFTP.Types hiding (get, put)

import Data.Char
import Control.Applicative

-- | TFTP message type.
data Message = RRQ String Mode |
               -- ^ Read request
               WRQ String Mode |
               -- ^ Write request
               DATA BlockNumber ByteString |
               -- ^ Data block with a raw bytestring
               ACK BlockNumber |
               -- ^ Acknowledge message
               Error TFTPError
               -- ^ Error message
          deriving(Read, Show, Ord, Eq)

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

    put (Error err)      = do
                          put (5 :: Word16)
                          put err

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

        3 -> DATA <$> get <*> (unDC <$> get)

        4 -> ACK <$> get

        5 -> Error <$> get

-- | The data type for block numbers in `Message's
type BlockNumber = Word16

-- | The data mode to encode the data with
data Mode =
    NetASCII |
    -- ^ "netascii" mode
    Octet
    -- ^ "octet" mode
          deriving(Read, Show, Ord, Eq)

instance Binary Mode where
    put NetASCII = put $ NString "netascii"
    put Octet = put $ NString "octet"

    get = do
      NString str <- get
      return $ case toLower <$> str of
                 "netascii" -> NetASCII
                 "octet" -> Octet

-- | Convert a `ByteString' encoded in `fromMode' to a `ByteString' encoded in
-- `toMode'.
convertMode :: Mode -> Mode -> ByteString -> ByteString
convertMode _ _ = id

-- | The error codes as defined in the RFC 1350
data TFTPError = ErrorMessage String |
                 -- ^ Encapsulates a custom message for a non-standard error
                 FileNotFound |
                 AccessViolation |
                 DiskFull |
                 IllegalTFTPOperation |
                 UnknownTransferID |
                 FileAlreadyExists |
                 NoSuchUser
          deriving(Read, Show, Ord, Eq)

getErrorCode :: TFTPError -> Word16
getErrorCode (ErrorMessage _str) = 0
getErrorCode FileNotFound = 1
getErrorCode AccessViolation = 2
getErrorCode DiskFull = 3
getErrorCode IllegalTFTPOperation = 4
getErrorCode UnknownTransferID = 5
getErrorCode FileAlreadyExists = 6
getErrorCode NoSuchUser = 7

getErrorMsg :: TFTPError -> NString
getErrorMsg (ErrorMessage str) = NString str
getErrorMsg _ = NString ""

makeTFTPError :: Word16 -> NString -> TFTPError
makeTFTPError 0 (NString msg) = ErrorMessage msg
makeTFTPError 1 _msg = FileNotFound
makeTFTPError 2 _msg = AccessViolation
makeTFTPError 3 _msg = DiskFull
makeTFTPError 4 _msg = IllegalTFTPOperation
makeTFTPError 5 _msg = UnknownTransferID
makeTFTPError 6 _msg = FileAlreadyExists
makeTFTPError 7 _msg = NoSuchUser

instance Binary TFTPError where
    put err = put (getErrorCode err) *> put (getErrorMsg err)
    get = makeTFTPError <$> get <*> get

newtype NString = NString String

nullTerminated :: String -> NString
nullTerminated = NString

data DataChunk = DC { unDC :: ByteString }

instance Binary DataChunk where
    put (DC bs) = mapM_ put (unpack bs)
    get = DC <$> getRemainingLazyByteString

instance Show NString where
    show (NString s) = s

instance Binary NString where
    put (NString str) = forM_ str put >> put ('\NUL':: Char)
    get = pure bsToNString <*> getLazyByteStringNul
        where
          bsToNString = NString . ((toEnum . fromIntegral) <$>) . unpack
