{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Version where

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

getVersion :: (MonadError e m, FromString e, MonadIO m, SendRecv s) => SwayT s m SwayVersion
getVersion = query GetVersion ""
