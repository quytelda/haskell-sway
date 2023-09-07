{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-|
Description : Types and functions common to other modules.

We can write computations that interact with the sway session using
the `Sway` monad or it's more general `SwayT` cousin.
-}
module System.Desktop.Sway.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.RWS
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.ByteString.Lazy           (ByteString)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

import           System.Desktop.Sway.Message    (Message)

-- | A monad transformer representing computations that occur within
-- the context of a sway IPC session.
--
-- This is more general than `Sway` monad, which assumes communication
-- occurs over a socket connection and requires `IO`. The transformer
-- allows complete substitution of the connection backend without
-- affecting `SwayT` computations.
type SwayT r w m = RWST r w [Message] m

-- | The `Sway` monad represents a computation within the context of a
-- sway IPC session.
type Sway = SwayT Socket String IO

-- | Unwrap a computation in the `SwayT` monad. This is the inverse of
-- `SwayT`. Returns the computation result or throws an error in the
-- base monad.
runSwayT :: SwayT r w m a -> r -> m (a, [Message], w)
runSwayT c r = runRWST c r []

-- | 'SendRecv' is a type class for objects that can send and receive
-- binary data in some monad, for example a `Socket` in the `IO`
-- monad.
class Monad m => SendRecv r m where
  send :: r -> ByteString -> m ()
  recv :: r -> m ByteString

instance SendRecv Socket IO where
  send = SocketBL.sendAll
  recv = SocketBL.getContents

-- | Fetch the connection object which represents the sway IPC
-- connection.
getConnection :: (Monoid w, Monad m) => SwayT r w m r
getConnection = ask

-- | A type class for error types that can be constructed from a
-- string. This enables polymorphic exception handling; we can throw
-- exceptions using String error messages and it will automatically be
-- converted to the proper type (for example, an `IOError` for the
-- `IO` monad).
--
-- See: https://stackoverflow.com/q/76963901/5129612
class FromString a where
  fromString :: String -> a

instance FromString String where
  fromString = id

instance FromString IOError where
  fromString = userError

-- | Throw an error using a String as an error message.
throwString :: (MonadError e m, FromString e) => String -> m a
throwString = throwError . fromString

-- | Lift an `Either String` into an `ErrorMonad e` where is some
-- `FromString` instance.
eitherToSway :: (MonadError e m, FromString e) => Either String a -> m a
eitherToSway = either throwString return

-- | Run a JSON parser in the SwayT monad.
parseSway :: (Monoid w, MonadError e m, FromString e) => (a -> Parser b) -> a -> SwayT r w m b
parseSway m = eitherToSway . parseEither m

-- | Decode a binary string containing JSON data.
swayDecode :: (Monoid w, MonadError e m, FromString e, FromJSON a) => ByteString -> SwayT r w m a
swayDecode = eitherToSway . eitherDecode
