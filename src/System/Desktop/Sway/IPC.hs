{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : Types and functions for communicating with the sway socket.

The sway IPC protocol uses a UNIX socket to communicate with sway as
well as other clients.
-}
module System.Desktop.Sway.IPC where

import           Control.Exception           (bracket)
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString)
import           Network.Socket
import           System.Environment          (lookupEnv)

import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Find the path to the sway socket, which is a UNIX socket file.
-- The socket path is stored in the @SWAYSOCK@ environment variable.
--
-- The @sway --get-socketpath@ command also returns the socket path,
-- but as of sway version 1.8.1, the command just reads the @SWAYSOCK@
-- variable.
findSwaySocket :: IO (Maybe FilePath)
findSwaySocket = lookupEnv "SWAYSOCK"

-- | Connect to a UNIX domain socket at the given file path.
-- A new `Socket` object will be created and returned.
-- The socket file must exist, otherwise an error will be thrown.
openUnixSocket :: FilePath -> IO Socket
openUnixSocket path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  return sock

-- | Close a UNIX socket connection.
-- Throws an error on failure.
closeUnixSocket :: Socket -> IO ()
closeUnixSocket = close'

-- | Execute an action that makes use of a UNIX socket at the given
-- path. The socket will be safely closed at the end regardless of
-- whether the operation was successful.
withUnixSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocket path = bracket (openUnixSocket path) closeUnixSocket

-- | Execute an action that uses the system sway socket.
-- If the sway socket cannot be located, raise an error.
withSwaySocket :: (Socket -> IO a) -> IO a
withSwaySocket f = findSwaySocket >>= \case
  Just path -> withUnixSocket path f
  Nothing   -> fail "Unable to get sway socket path."

-- | Send bytes to the sway socket.
sendBytes :: SendRecv s m => ByteString -> SwayT s m ()
sendBytes bytes = getConnection >>= lift . flip send bytes

-- | Receive bytes from the sway socket.
recvBytes :: SendRecv s m => SwayT s m ByteString
recvBytes = getConnection >>= lift . recv

-- | Send a `Message` over the sway socket.
sendMessage :: SendRecv s m => Message -> SwayT s m ()
sendMessage = sendBytes . msgEncode

-- | Receive a `Message` from the sway socket.
-- Throws an exception if the message parsing fails.
recvMessage :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m Message
recvMessage = recvBytes >>= eitherToSway . msgDecode

-- | Send an IPC message and receive the reply.
ipc :: (MonadError e m, FromString e, SendRecv s m) => Message -> SwayT s m Message
ipc msg = sendMessage msg >> recvMessage

-- | Send a sway IPC message, receive the reply, and parse it's payload.
-- Construct the outgoing IPC message with the given type and payload.
-- Fail if the outgoing and incoming types don't match.
query :: (MonadError e m, FromString e, SendRecv s m, FromJSON a) =>
         MessageType -> ByteString -> SwayT s m a
query type1 bytes = do
  reply <- ipc $ Message type1 bytes
  case reply of
    Message type2 payload
      | type1 == type2 -> swayDecode payload
    _                  -> throwString $ "expected " <> show type1 <> " reply"

-- | Subscribe to IPC events.
-- Request to receive any events of the given types from sway.
subscribe :: (MonadError e m, FromString e, SendRecv s m) => [EventType] -> SwayT s m ()
subscribe events = do
  success <- query SUBSCRIBE (encode events)
             >>= parseSway result
  unless success $
    throwString $ "subscribing failed: " <> show events
  where
    result = withObject "success" (.: "success")

-- | An event generated whenever a client sends a tick.
--
-- This event may carry an arbitrary payload.
data TickEvent = TickEvent { tickFirst   :: Bool
                           , tickPayload :: String
                           } deriving (Eq, Show)

instance FromJSON TickEvent where
  parseJSON = withObject "TickEvent" $ \obj -> do
    tickFirst   <- obj .: "first"
    tickPayload <- obj .: "payload"

    return TickEvent{..}

-- | Sends a TICK event with the given payload.
--
-- NOTE: This function sends a ByteString payload because the payload
-- is embedded directly into a binary `Message`. However, it will be
-- distributed to clients as a JSON string, so beware encoding issues.
sendTick :: (MonadError e m, FromString e, SendRecv s m) => ByteString -> SwayT s m Bool
sendTick payload = query SEND_TICK payload >>= parseSway (.: "success")

-- | Sends a SYNC message.
--
-- SYNC messages only exist in sway for i3 compatibility, and the
-- query always returns a failure status.
sync :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m Bool
sync = query SYNC "" >>= parseSway (.: "success")
