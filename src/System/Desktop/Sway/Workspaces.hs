{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : IPC functionality related to workspaces.
-}
module System.Desktop.Sway.Workspaces where

import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Rectangle
import           System.Desktop.Sway.Types

-- | A description of a sway workspace.
--
-- A JSON description of a workspace can be returned either from a
-- GET_WORKSPACES query or a WORKSPACE event.
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
    wsFocused <- obj .: "focused"
    wsUrgent  <- obj .: "urgent"
    wsRect    <- obj .: "rect"
    wsOutput  <- obj .: "output"
    wsVisible <- obj .:? "visible" .!= False

    return Workspace{..}

-- | An event generated when a workspace-related event occurs.
--
-- If present, the first Workspace parameter represents the affected
-- workspace. For focus events, the second parameter represents the
-- previously focused workspace.
data WorkspaceEvent = WorkspaceInit   Workspace
                    | WorkspaceEmpty  Workspace
                    | WorkspaceFocus  Workspace Workspace
                    | WorkspaceMove   Workspace
                    | WorkspaceRename Workspace
                    | WorkspaceUrgent Workspace
                    | WorkspaceReload
                    deriving (Eq, Show)

instance FromJSON WorkspaceEvent where
  parseJSON = withObject "WorkspaceEvent" $ \obj -> do
    change  <- obj .: "change"

    case change of
      "reload" -> return WorkspaceReload
      "init"   -> WorkspaceInit   <$> obj .: "current"
      "empty"  -> WorkspaceEmpty  <$> obj .: "current"
      "move"   -> WorkspaceMove   <$> obj .: "current"
      "rename" -> WorkspaceRename <$> obj .: "current"
      "urgent" -> WorkspaceUrgent <$> obj .: "current"
      "focus"  -> WorkspaceFocus  <$> obj .: "current" <*> obj .: "old"
      _        -> fail $ "Unrecognized workspace event: " <> change

-- | Get the list of sway workspaces.
-- Send a `GET_WORKSPACES` IPC message and return the parsed results.
getWorkspaces :: (Monoid w, MonadError e m, FromString e, SendRecv r m) => SwayT r w m [Workspace]
getWorkspaces = query GET_WORKSPACES ""
