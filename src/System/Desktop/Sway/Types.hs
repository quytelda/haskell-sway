{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.ByteString.Lazy           (ByteString)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

import           System.Desktop.Sway.Exception  hiding (fromString)

-- | The sway monad encapsulates a computation within the context of a
-- sway IPC connection.
type SwayT s m = ReaderT s m
type Sway = SwayT Socket IO

-- | Unwrap a computation in the Sway monad.
-- Returns either the computation result or an error message in the base monad.
runSwayT :: SwayT s m a -> s -> m a
runSwayT = runReaderT

class SendRecv a where
  send :: a -> ByteString -> IO ()
  recv :: a -> IO ByteString

instance SendRecv Socket where
  send = SocketBL.sendAll
  recv = SocketBL.getContents

-- | Fetch the connection object within the monad.
getConnection :: MonadIO m => SwayT s m s
getConnection = ask

eitherToSway :: (MonadError e m, FromString e) => Either String a -> m a
eitherToSway = either throwString return

parseSway :: (MonadError e m, FromString e) => (a -> Parser b) -> a -> SwayT s m b
parseSway m = eitherToSway . parseEither m

swayDecode :: (FromJSON a, MonadError e m, FromString e) => ByteString -> SwayT s m a
swayDecode = eitherToSway . eitherDecode
