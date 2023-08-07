{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Types where

import           Control.Monad.Trans            (MonadIO, lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.ByteString.Lazy           (ByteString)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

-- | The sway monad encapsulates a computation within the context of a
-- sway IPC connection.
type SwayT s m = ExceptT String (ReaderT s m)
type Sway = SwayT Socket IO

-- | Unwrap a computation in the sway monad.
-- Returns either the computation result or an error message in the base monad.
runSwayT :: SwayT s m a -> s -> m (Either String a)
runSwayT = runReaderT . runExceptT

class SendRecv a where
  send :: a -> ByteString -> IO ()
  recv :: a -> IO ByteString

instance SendRecv Socket where
  send = SocketBL.sendAll
  recv = SocketBL.getContents

-- | Fetch the connection object within the monad.
getConnection :: MonadIO m => SwayT s m s
getConnection = lift ask

parseSway :: Monad m => (a -> Parser b) -> a -> SwayT s m b
parseSway m = except . parseEither m

swayDecode :: (FromJSON a, Monad m) => ByteString -> SwayT s m a
swayDecode = except . eitherDecode

data Rectangle = Rectangle { rectX      :: Int
                           , rectY      :: Int
                           , rectWidth  :: Int
                           , rectHeight :: Int
                           } deriving (Eq, Show)

instance FromJSON Rectangle where
  parseJSON = withObject "Rectangle" $ \obj -> do
    rectX      <- obj .: "x"
    rectY      <- obj .: "y"
    rectWidth  <- obj .: "width"
    rectHeight <- obj .: "height"

    return Rectangle{..}

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

data OutputMode = OutputMode { modeWidth   :: Int
                             , modeHeight  :: Int
                             , modeRefresh :: Int
                             } deriving (Eq, Show)

instance FromJSON OutputMode where
  parseJSON = withObject "OutputMode" $ \obj -> do
    modeWidth   <- obj .: "width"
    modeHeight  <- obj .: "height"
    modeRefresh <- obj .: "refresh"

    return OutputMode{..}

data Output = Output { outputName             ::  String
                     , outputMake             ::  String
                     , outputModel            ::  String
                     , outputSerial           ::  String
                     , outputActive           ::  Bool
                     , outputDPMS             ::  Bool
                     , outputPower            ::  Bool
                     , outputPrimary          ::  Bool
                     , outputScale            ::  Double
                     , outputSubpixelHinting  ::  String
                     , outputTransform        ::  String
                     , outputCurrentWorkspace ::  String
                     , outputModes            ::  [OutputMode]
                     , outputCurrentMode      ::  OutputMode
                     , outputRect             ::  Rectangle
                     } deriving (Eq, Show)

instance FromJSON Output where
  parseJSON = withObject "Output" $ \obj -> do
    outputName             <- obj .: "name"
    outputMake             <- obj .: "make"
    outputModel            <- obj .: "model"
    outputSerial           <- obj .: "serial"
    outputActive           <- obj .: "active"
    outputDPMS             <- obj .: "dpms"
    outputPower            <- obj .: "power"
    outputPrimary          <- obj .: "primary"
    outputScale            <- obj .: "scale"
    outputSubpixelHinting  <- obj .: "subpixel_hinting"
    outputTransform        <- obj .: "transform"
    outputCurrentWorkspace <- obj .: "current_workspace"
    outputModes            <- obj .: "modes"
    outputCurrentMode      <- obj .: "current_mode"
    outputRect             <- obj .: "rect"

    return Output{..}

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
