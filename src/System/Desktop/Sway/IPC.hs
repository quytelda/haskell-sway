{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.IPC where

import           Control.Exception  (bracket)
import           Network.Socket
import           System.Environment (lookupEnv)

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
