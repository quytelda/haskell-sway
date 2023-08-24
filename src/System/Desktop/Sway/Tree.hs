{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Tree where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types              (Parser)

import           System.Desktop.Sway.Exception
import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Rectangle
import           System.Desktop.Sway.Types

data NodeType = RootNode
              | OutputNode
              | WorkspaceNode
              | ContainerNode
              | FloatingNode
              deriving (Eq, Show)

parseNodeType :: String -> Parser NodeType
parseNodeType "root"         = return RootNode
parseNodeType "output"       = return OutputNode
parseNodeType "workspace"    = return WorkspaceNode
parseNodeType "con"          = return ContainerNode
parseNodeType "floating_con" = return FloatingNode
parseNodeType _              = fail "invalid node type"

data Node = Node { nodeID                 :: Int
                 , nodeName               :: Maybe String
                 , nodeType               :: NodeType
                 , nodeBorder             :: String
                 , nodeCurrentBorderWidth :: Int
                 , nodeLayout             :: String
                 , nodeOrientation        :: String
                 , nodePercent            :: Double
                 , nodeRect               :: Rectangle
                 , nodeWindowRect         :: Rectangle
                 , nodeDecoRect           :: Rectangle
                 , nodeGeometry           :: Rectangle
                 , nodeUrgent             :: Bool
                 , nodeSticky             :: Bool
                 , nodeMarks              :: [String]
                 , nodeFocused            :: Bool
                 , nodeFocus              :: [Int]
                 , nodeNodes              :: [Node]
                 , nodeFloatingNodes      :: [Node]
                 } deriving (Eq, Show)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \obj -> do
    nodeID                 <- obj .: "id"
    nodeName               <- obj .: "name"
    nodeType               <- obj .: "type" >>= parseNodeType
    nodeBorder             <- obj .: "border"
    nodeCurrentBorderWidth <- obj .: "current_border_width"
    nodeLayout             <- obj .: "layout"
    nodeOrientation        <- obj .: "orientation"
    nodePercent            <- obj .: "percent"
    nodeRect               <- obj .: "rect"
    nodeWindowRect         <- obj .: "window_rect"
    nodeDecoRect           <- obj .: "deco_rect"
    nodeGeometry           <- obj .: "geometry"
    nodeUrgent             <- obj .: "urgent"
    nodeSticky             <- obj .: "sticky"
    nodeMarks              <- obj .: "marks"
    nodeFocused            <- obj .: "focused"
    nodeFocus              <- obj .: "focus"
    nodeNodes              <- obj .: "nodes"
    nodeFloatingNodes      <- obj .: "floating_nodes"

    return Node{..}

-- | Get the current layout tree.
-- Send a `GET_TREE` IPC message and return the parsed result.
getTree :: (MonadError e m, FromString e, MonadIO m, SendRecv s) => SwayT s m Node
getTree = query GetTree ""
