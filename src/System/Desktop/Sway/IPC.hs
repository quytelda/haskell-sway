{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.IPC where

import           Control.Exception          (bracket)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.ByteString.Lazy       (ByteString)
import           Network.Socket
import           System.Environment         (lookupEnv)

import           System.Desktop.Sway.Types


-- | Find the path to the sway socket file.
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

-- | Send bytes using the connection object within the monad.
sendBytes :: (MonadIO m, SendRecv s) => ByteString -> SwayT s m ()
sendBytes bytes = getConnection >>= liftIO . flip send bytes

-- | Receive bytes using the connection object within the monad.
recvBytes :: (MonadIO m, SendRecv s) => SwayT s m ByteString
recvBytes = getConnection >>= liftIO . recv

-- | Send a Message using the connection object within the monad.
sendMessage :: (MonadIO m, SendRecv s) => Message -> SwayT s m ()
sendMessage = sendBytes . msgEncode

-- | Receive a Message using the connection object within the monad.
-- Throws an exception if the received message cannot be parsed.
recvMessage :: (MonadIO m, SendRecv s) => SwayT s m Message
recvMessage = do
  bytes <- recvBytes
  case msgDecode bytes of
    Left  err -> throwE err
    Right msg -> return msg

-- | Send an IPC message and receive the reply.
ipc :: (MonadIO m, SendRecv s) => Message -> SwayT s m Message
ipc msg = sendMessage msg >> recvMessage
