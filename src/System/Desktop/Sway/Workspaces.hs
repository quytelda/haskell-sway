{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Workspaces where

import           Control.Monad.Trans        (MonadIO)

import           System.Desktop.Sway.Types
import           System.Desktop.Sway.IPC

-- | Get the list of sway workspaces.
-- Send a `GET_WORKSPACES` IPC message and return the parsed results.
getWorkspaces :: (MonadIO m, SendRecv s) => SwayT s m [Workspace]
getWorkspaces = query GetWorkspaces ""
