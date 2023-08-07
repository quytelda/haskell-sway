{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Types where

import           Control.Monad.Trans            (MonadIO, lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.ByteString.Lazy           (ByteString)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

-- | The sway monad encapsulates a computation within the context of a
-- sway IPC connection.
type SwayT s m = ExceptT String (ReaderT s m)
type Sway = SwayT Socket IO

-- | Unwrap a computation in the sway monad.
-- Returns either the computation result or an error message in the base monad.
runSwayT :: SwayT s m a -> s -> m (Either String a)
runSwayT = runReaderT . runExceptT

class SendRecv a where
  send :: a -> ByteString -> IO ()
  recv :: a -> IO ByteString

instance SendRecv Socket where
  send = SocketBL.sendAll
  recv = SocketBL.getContents

-- | Fetch the connection object within the monad.
getConnection :: MonadIO m => SwayT s m s
getConnection = lift ask

parseSway :: Monad m => (a -> Parser b) -> a -> SwayT s m b
parseSway m = except . parseEither m

swayDecode :: (FromJSON a, Monad m) => ByteString -> SwayT s m a
swayDecode = except . eitherDecode
