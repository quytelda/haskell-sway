{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Bar where

import           Control.Monad.Trans         (MonadIO)
import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

data BarConfig = BarConfig { barId :: String }

instance FromJSON BarConfig where
  parseJSON = withObject "BarConfig" $ \obj -> do
    barId <- obj .: "id"

    return BarConfig{..}

-- | Get the list of marks currently set.
getBarConfig :: (MonadIO m, SendRecv s) => ByteString -> SwayT s m [String]
getBarConfig barID = query GetBarConfig barID

getBars :: (MonadIO m, SendRecv s) => SwayT s m BarConfig
getBars = query GetBarConfig ""
