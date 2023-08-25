{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Workspaces where

import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.Exception
import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Rectangle
import           System.Desktop.Sway.Types

data Workspace = Workspace { wsNum     :: Int
                           , wsName    :: String
                           , wsVisible :: Bool
                           , wsFocused :: Bool
                           , wsUrgent  :: Bool
                           , wsRect    :: Rectangle
                           , wsOutput  :: String
                           } deriving (Eq, Show)

instance FromJSON Workspace where
  parseJSON = withObject "Workspace" $ \obj -> do
    wsNum     <- obj .: "num"
    wsName    <- obj .: "name"
    wsVisible <- obj .: "visible"
    wsFocused <- obj .: "focused"
    wsUrgent  <- obj .: "urgent"
    wsRect    <- obj .: "rect"
    wsOutput  <- obj .: "output"

    return Workspace{..}

-- | Get the list of sway workspaces.
-- Send a `GET_WORKSPACES` IPC message and return the parsed results.
getWorkspaces :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [Workspace]
getWorkspaces = query GetWorkspaces ""
