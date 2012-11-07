-- | Common Types used internally
module
    Network.TFTP.Types ( ByteString(..)
                       , bpack
                       , bunpack
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

type ByteString = B.ByteString

bpack = B.pack
bunpack = B.unpack
bdrop = B.drop
btake = B.take
blength = B.length

-- | Type class for monads that can send/receive messages
class  (Eq address, Show address, Monad m, MonadIO m) =>
    MessageIO m address | m -> address where

    -- | send a message, always succeeds even if there was an error
    sendTo :: address -> ByteString -> m ()

    -- | receive a message, failing if no message was receive after a timeout
    -- (measured in seconds)
    receiveFrom :: Maybe Int -> m (Maybe (address, ByteString))

    -- | return the address that 'receiveFrom' receives on
    localAddress :: m address
