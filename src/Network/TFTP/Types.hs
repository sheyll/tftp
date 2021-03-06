-- | Common types used internally. Re-exports the 'ByteString' type to use as well as some monad transformer stuff, exceptions, logging, 'Data.Word' and printf.
module
    Network.TFTP.Types ( ByteString(..)
                       , B.pack
                       , B.unpack
                       , bdrop
                       , btake
                       , blength
                       , B.readFile

                       , MessageIO(..)

                       , module Control.Monad
                       , module Control.Applicative
                       , module Control.Monad.IO.Class
                       , module Control.Monad.State.Class
                       , module Control.Monad.Trans.State.Strict
                       , module Control.Monad.Trans.Class

                       , module Control.Exception

                       , module System.Log.Logger
                       , module System.Log.Handler.Simple
                       , module Data.Word
                       , module Text.Printf
                       )
    where

import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, State, evalState)
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Exception(assert)

import System.Log.Logger
import System.Log.Handler.Simple

import Data.Word( Word8
                , Word16)

import Text.Printf(printf)

-- | Alias for the Lazy ByteString that is used internally
type ByteString = B.ByteString

-- | Candy for ByteString.pack to not interfere with Prelude(drop)
bdrop = B.drop

-- | Candy for ByteString.take
btake = B.take

-- | Candy for ByteString.length
blength = B.length

-- | Type class for monads that can send/receive messages
class  (Eq address, Show address, Monad m, MonadIO m) =>
    MessageIO m address | m -> address where

    -- | Send a message or return 'False' when there was an error
    sendTo :: address -> ByteString -> m Bool

    -- | Receive a message, failing if no message was receive after a timeout
    -- (measured in seconds)
    receiveFrom :: Maybe Int -> m (Maybe (address, ByteString))

    -- | Return the address that 'receiveFrom' receives on
    localAddress :: m address
