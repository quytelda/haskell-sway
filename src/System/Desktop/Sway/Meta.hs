{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Meta where

import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.Exception
import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

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

getVersion :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m SwayVersion
getVersion = query GET_VERSION ""

getConfig :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m String
getConfig = query GET_CONFIG "" >>= parseSway (.: "config")
