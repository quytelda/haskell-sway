{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Marks where

import           Control.Monad.Except

import           System.Desktop.Sway.Exception
import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Get the list of marks currently set.
getMarks :: (MonadError e m, FromString e, MonadIO m, SendRecv s) => SwayT s m [String]
getMarks = query GetMarks ""
