{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : IPC functionality related to the view tree.
-}
module System.Desktop.Sway.Tree where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types              (Parser)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Rectangle
import           System.Desktop.Sway.Types

-- | A type of view.
data NodeType = RootNode
              | OutputNode
              | WorkspaceNode
              | ContainerNode
              | FloatingNode
              deriving (Eq, Show)

-- | Parse a view type from a String.
parseNodeType :: String -> Parser NodeType
parseNodeType "root"         = return RootNode
parseNodeType "output"       = return OutputNode
parseNodeType "workspace"    = return WorkspaceNode
parseNodeType "con"          = return ContainerNode
parseNodeType "floating_con" = return FloatingNode
parseNodeType _              = fail "invalid node type"

-- | A node in the view tree describing a particular view.
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
getTree :: (Monoid w, MonadError e m, FromString e, SendRecv r m) => SwayT r w m Node
getTree = query GET_TREE ""

-- | Get the list of marks currently set.
getMarks :: (Monoid w, MonadError e m, FromString e, SendRecv r m) => SwayT r w m [String]
getMarks = query GET_MARKS ""

-- | An event generated when a view-related change occurs.
data WindowEvent = WindowNew            Node
                 | WindowClose          Node
                 | WindowFocus          Node
                 | WindowTitle          Node
                 | WindowFullscreenMode Node
                 | WindowMove           Node
                 | WindowFloating       Node
                 | WindowUrgent         Node
                 | WindowMark           Node
                 deriving (Eq, Show)

instance FromJSON WindowEvent where
  parseJSON = withObject "WindowEvent" $ \obj -> do
    change    <- obj .: "change"

    let event = case change of
          "new"             -> return WindowNew
          "close"           -> return WindowClose
          "focus"           -> return WindowFocus
          "title"           -> return WindowTitle
          "fullscreen_mode" -> return WindowFullscreenMode
          "move"            -> return WindowMove
          "floating"        -> return WindowFloating
          "urgent"          -> return WindowUrgent
          "mark"            -> return WindowMark
          _                 -> fail $ "Unrecognized window event: " <> change

    event <*> obj .: "container"
