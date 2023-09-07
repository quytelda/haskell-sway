{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


{-|
Description : IPC functionality related to status bars.
-}
module System.Desktop.Sway.Bar where

import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.Map.Strict             as Map

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | A representation of a status bar configuration.
--
-- Different status bars may or may not support reporting information
-- about their configuration this way.
--
-- TODO: Implement more detailed types for barColors and barGaps
data BarConfig = BarConfig { barId                   :: String
                           , barMode                 :: String
                           , barPosition             :: String
                           , barStatusCommand        :: String
                           , barFont                 :: String
                           , barWorkspaceButtons     :: Bool
                           , barWorkspaceMinWidth    :: Int
                           , barBindingModeIndicator :: Bool
                           , barVerbose              :: Bool
                           , barColors               :: Map.Map String String
                           , barGaps                 :: Map.Map String Int
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
getBarConfig :: (Monoid w, MonadError e m, FromString e, SendRecv r m) => ByteString -> SwayT r w m BarConfig
getBarConfig barID = query GET_BAR_CONFIG barID

-- | Return a list of the currently active status bars.
--
-- The IDs returned from this query can be used to request further
-- information about a bar using `getBarConfig` or match status bar
-- events affecting a particular bar.
getBarIDs :: (Monoid w, MonadError e m, FromString e, SendRecv r m) => SwayT r w m [String]
getBarIDs = query GET_BAR_CONFIG ""

-- | An event generated whenever the visibility of a bar changes.
data BarStateUpdateEvent
  = BarStateUpdateEvent { barStateId                :: String
                        , barStateVisibleByModifier :: Bool
                        } deriving (Eq, Show)

instance FromJSON BarStateUpdateEvent where
  parseJSON = withObject "BarStateUpdateEvent" $ \obj -> do
    barStateId                <- obj .: "id"
    barStateVisibleByModifier <- obj .: "visible_by_modifier"

    return BarStateUpdateEvent{..}
