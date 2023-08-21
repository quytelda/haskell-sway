{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Input where

import           Control.Monad.Trans         (MonadIO)
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

getBindingModes :: (MonadIO m, SendRecv s) => SwayT s m [String]
getBindingModes = query GetBindingModes ""

getBindingState :: (MonadIO m, SendRecv s) => SwayT s m String
getBindingState = query GetBindingState "" >>= parseSway (.: "name")
