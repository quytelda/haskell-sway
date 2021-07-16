{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Output where

import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (except, throwE)
import           Data.Aeson                 (eitherDecode)

import           System.Desktop.Sway.Types
import           System.Desktop.Sway.IPC

-- | Get the list of sway outputs.
-- Send a `GET_OUTPUTS` IPC message and return the parsed results.
getOutputs :: (MonadIO m, SendRecv s) => SwayT s m [Output]
getOutputs = query GetOutputs ""
