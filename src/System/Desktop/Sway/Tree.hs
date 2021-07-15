{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Tree where

import           Control.Monad.Trans       (MonadIO)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Types

-- | Get the current tree of Nodes.
getTree :: (MonadIO m, SendRecv s) => SwayT s m Node
getTree = query GetTree ""
