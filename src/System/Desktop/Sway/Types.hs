{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Description : Types and functions common to other modules.
-}
module System.Desktop.Sway.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.ByteString.Lazy           (ByteString)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

import           System.Desktop.Sway.Exception  hiding (fromString)

-- | The `Sway` monad represents a computation within the context of a
-- sway IPC session.
type SwayT s m = ReaderT s m
type Sway = SwayT Socket IO

-- | Unwrap a computation in the `SwayT` monad. This is the inverse of
-- `SwayT`. Returns the computation result or throws an error in the
-- base monad.
runSwayT :: SwayT s m a -> s -> m a
runSwayT = runReaderT

-- | 'SendRecv' is a type class for objects that can send and receive
-- binary data in some monad, for example a `Socket` in the `IO`
-- monad.
class Monad m => SendRecv s m where
  send :: s -> ByteString -> m ()
  recv :: s -> m ByteString

instance SendRecv Socket IO where
  send = SocketBL.sendAll
  recv = SocketBL.getContents

-- | Fetch the connection object which represents the sway IPC
-- connection.
getConnection :: Monad m => SwayT s m s
getConnection = ask

-- | Lift an `Either String` into an `ErrorMonad e` where is some
-- `FromString` instance.
eitherToSway :: (MonadError e m, FromString e) => Either String a -> m a
eitherToSway = either throwString return

-- | Run a JSON parser in the SwayT monad.
parseSway :: (MonadError e m, FromString e) => (a -> Parser b) -> a -> SwayT s m b
parseSway m = eitherToSway . parseEither m

-- | Decode a binary string containing JSON data.
swayDecode :: (MonadError e m, FromString e, FromJSON a) => ByteString -> SwayT s m a
swayDecode = eitherToSway . eitherDecode
