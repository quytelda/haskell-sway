{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Tree where

import           Control.Monad.Trans         (MonadIO)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Types
import           System.Desktop.Sway.Message

-- | Get the current layout tree.
-- Send a `GET_TREE` IPC message and return the parsed result.
getTree :: (MonadIO m, SendRecv s) => SwayT s m Node
getTree = query GetTree ""
