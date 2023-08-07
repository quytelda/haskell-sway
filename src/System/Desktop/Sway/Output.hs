{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Output where

import           Control.Monad.Trans         (MonadIO)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Get the list of sway outputs.
-- Send a `GET_OUTPUTS` IPC message and return the parsed results.
getOutputs :: (MonadIO m, SendRecv s) => SwayT s m [Output]
getOutputs = query GetOutputs ""
