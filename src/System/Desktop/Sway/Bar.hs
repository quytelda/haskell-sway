{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Bar where

import           Control.Monad.Trans         (MonadIO)
import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

data BarConfig = BarConfig { barId                   :: String
                           , barMode                 :: String
                           , barPosition             :: String
                           , barStatusCommand        :: String
                           , barFont                 :: String
                           , barWorkspaceButtons     :: Bool
                           , barWorkspaceMinWidth    :: Int
                           , barBindingModeIndicator :: Bool
                           , barVerbose              :: Bool
                           , barColors               :: Object
                           , barGaps                 :: Object
                           , barHeight               :: Int
                           , barStatusPadding        :: Int
                           , barStatusEdgePadding    :: Int
                           } deriving (Eq, Show)

instance FromJSON BarConfig where
  parseJSON = withObject "BarConfig" $ \obj -> do
    barId                   <- obj .: "id"
    barMode                 <- obj .: "mode"
    barPosition             <- obj .: "position"
    barStatusCommand        <- obj .: "status_command"
    barFont                 <- obj .: "font"
    barWorkspaceButtons     <- obj .: "workspace_buttons"
    barWorkspaceMinWidth    <- obj .: "workspace_min_width"
    barBindingModeIndicator <- obj .: "binding_mode_indicator"
    barVerbose              <- obj .: "verbose"
    barColors               <- obj .: "colors"
    barGaps                 <- obj .: "gaps"
    barHeight               <- obj .: "bar_height"
    barStatusPadding        <- obj .: "status_padding"
    barStatusEdgePadding    <- obj .: "status_edge_padding"

    return BarConfig{..}

-- | Get the list of marks currently set.
getBarConfig :: (MonadIO m, SendRecv s) => ByteString -> SwayT s m [String]
getBarConfig barID = query GetBarConfig barID

getBars :: (MonadIO m, SendRecv s) => SwayT s m BarConfig
getBars = query GetBarConfig ""
