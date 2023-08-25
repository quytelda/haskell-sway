{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.IPC where

import           Control.Exception             (bracket)
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy          (ByteString)
import           Network.Socket
import           System.Environment            (lookupEnv)

import           System.Desktop.Sway.Exception hiding (fromString)
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Find the path to the sway socket file.
-- The path is stored in the $SWAYSOCK environment variable.
getSocketPath :: IO (Maybe FilePath)
getSocketPath = lookupEnv "SWAYSOCK"

-- | Connect to a UNIX domain socket at the given file path.
-- The socket must exist.
openUnixSocket :: FilePath -> IO Socket
openUnixSocket path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  return sock

-- | Close the connection with a UNIX socket.
-- Throws an error on failure.
closeUnixSocket :: Socket -> IO ()
closeUnixSocket = close'

-- | Execute an action that makes use of a UNIX socket at the given path.
withUnixSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocket path = bracket (openUnixSocket path) closeUnixSocket

-- | Execute an action that uses the system sway socket.
-- Fails if the sway socket cannot be located.
withSwaySocket :: (Socket -> IO a) -> IO a
withSwaySocket f = getSocketPath >>= \case
  Just path -> withUnixSocket path f
  Nothing   -> fail "Unable to get sway socket path."

-- | Send bytes using the connection object within the monad.
sendBytes :: SendRecv s m => ByteString -> SwayT s m ()
sendBytes bytes = getConnection >>= lift . flip send bytes

-- | Receive bytes using the connection object within the monad.
recvBytes :: SendRecv s m => SwayT s m ByteString
recvBytes = getConnection >>= lift . recv

-- | Send a Message using the connection object within the monad.
sendMessage :: SendRecv s m => Message -> SwayT s m ()
sendMessage = sendBytes . msgEncode

-- | Receive a Message using the connection object within the monad.
-- Throws an exception if the received message cannot be parsed.
recvMessage :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m Message
recvMessage = do
  bytes <- recvBytes
  case msgDecode bytes of
    Left  err -> throwString err
    Right msg -> return msg

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
  success <- query Subscribe (encode events)
             >>= parseSway result
  unless success $
    throwString $ "subscribing failed: " <> show events
  where
    result = withObject "success" (.: "success")
