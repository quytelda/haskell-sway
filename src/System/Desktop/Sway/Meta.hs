{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : General-purpose sway queries concerning sway itself.
-}
module System.Desktop.Sway.Meta where

import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Information about the sway version
data SwayVersion = SwayVersion { versionMajor         :: Int
                               , versionMinor         :: Int
                               , versionPatch         :: Int
                               , versionHumanReadable :: String
                               , versionConfigPath    :: FilePath
                               } deriving (Eq, Show)

instance FromJSON SwayVersion where
  parseJSON = withObject "SwayVersion" $ \obj -> do
    versionMajor         <- obj .: "major"
    versionMinor         <- obj .: "minor"
    versionPatch         <- obj .: "patch"
    versionHumanReadable <- obj .: "human_readable"
    versionConfigPath    <- obj .: "loaded_config_file_name"

    return SwayVersion{..}

-- | Query the sway daemon about its version.
getVersion :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m SwayVersion
getVersion = query GET_VERSION ""

-- | Query the contents of the currently active sway configuration.
getConfig :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m String
getConfig = query GET_CONFIG "" >>= parseSway (.: "config")

-- | An event generated when IPC is shutting down.
--
-- Currently, the event has only one variant ("exit").
data ShutdownEvent = ShutdownExit
                   deriving (Eq, Show)

instance FromJSON ShutdownEvent where
  parseJSON = withObject "ShutdownEvent" $ \obj -> do
    change <- obj .: "change"
    case change of
      "exit" -> return ShutdownExit
      _      -> fail $ "Unrecognized shutdown event: " <> change
