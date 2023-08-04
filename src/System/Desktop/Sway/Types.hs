{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans            (MonadIO, lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types               (Parser, parseEither)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BL
import           Data.List                      (find)
import           Data.Maybe                     (fromJust)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as SocketBL

data MessageType = RunCommand
                 | GetWorkspaces
                 | Subscribe
                 | GetOutputs
                 | GetTree
                 | GetMarks
                 | GetBarConfig
                 | GetVersion
                 | GetBindingModes
                 | GetConfig
                 | SendTick
                 | Sync
                 | GetBindingState
                 | GetInputs
                 | GetSeats
                 deriving (Eq, Show)

data EventType = WorkspaceEvent
               | ModeEvent
               | WindowEvent
               | BarConfigUpdateEvent
               | BindingEvent
               | ShutdownEvent
               | TickEvent
               | BarStateUpdateEvent
               | InputEvent
               deriving (Eq, Show)

instance ToJSON EventType where
  toJSON WorkspaceEvent       = "workspace"
  toJSON ModeEvent            = "mode"
  toJSON WindowEvent          = "window"
  toJSON BarConfigUpdateEvent = "barconfig_update"
  toJSON BindingEvent         = "binding"
  toJSON ShutdownEvent        = "shutdown"
  toJSON TickEvent            = "tick"
  toJSON BarStateUpdateEvent  = "bar_state_update"
  toJSON InputEvent           = "input"

msgCodes :: [(MessageType, Word32)]
msgCodes =
  [ (RunCommand,      0x00)
  , (GetWorkspaces,   0x01)
  , (Subscribe,       0x02)
  , (GetOutputs,      0x03)
  , (GetTree,         0x04)
  , (GetMarks,        0x05)
  , (GetBarConfig,    0x06)
  , (GetVersion,      0x07)
  , (GetBindingModes, 0x08)
  , (GetConfig,       0x09)
  , (SendTick,        0x0a)
  , (Sync,            0x0b)
  , (GetBindingState, 0x0c)
  , (GetInputs,       0x64)
  , (GetSeats,        0x65)
  ]

evtCodes :: [(EventType, Word32)]
evtCodes =
  [ (WorkspaceEvent,       0x80000000)
  , (ModeEvent,            0x80000002)
  , (WindowEvent,          0x80000003)
  , (BarConfigUpdateEvent, 0x80000004)
  , (BindingEvent,         0x80000005)
  , (ShutdownEvent,        0x80000006)
  , (TickEvent,            0x80000007)
  , (BarStateUpdateEvent,  0x80000014)
  , (InputEvent,           0x80000015)
  ]

lookupRev :: (Foldable t, Eq a) => a -> t (b, a) -> Maybe b
lookupRev c t = fst <$> find (\p -> c == snd p) t

data Message = Message MessageType ByteString
             | Event   EventType   ByteString
             deriving (Eq, Show)

-- | Get the payload data of a Message.
msgPayload :: Message -> ByteString
msgPayload (Message _ bytes) = bytes
msgPayload (Event   _ bytes) = bytes

-- | Get the size in bytes of the message payload.
msgLength :: Num a => Message -> a
msgLength = fromIntegral . BL.length . msgPayload

-- | Every IPC message is prefixed with the magic string "i3-ipc".
magicString :: ByteString
magicString = "i3-ipc"

-- | Get the payload type code as a 32-bit integer.
msgTypeCode :: Message -> Word32
msgTypeCode (Message t _) = fromJust . lookup t $ msgCodes
msgTypeCode (Event   t _) = fromJust . lookup t $ evtCodes

-- | Serialize a message.
putMessage :: Message -> Put
putMessage msg = do
  putLazyByteString magicString
  putWord32host (msgLength msg)
  putWord32host (msgTypeCode msg)
  putLazyByteString (msgPayload msg)

-- | Consume the magic string that begins every IPC message.
-- Fail if the consumed bytes do not match.
getMagicString :: Get ()
getMagicString = do
  bytes <- getLazyByteString (BL.length magicString)
  when (bytes /= magicString) $
    fail "Expected magic string."

-- | Construct a message from a type code and a payload.
-- Returns Nothing if the given type code is unrecognized.
mkMessage :: Word32 -> ByteString -> Maybe Message
mkMessage code payload =
  let msg = Message <$> lookupRev code msgCodes
      evt = Event   <$> lookupRev code evtCodes
  in (msg <|> evt) <*> pure payload

-- | Deserialize a message.
getMessage :: Get Message
getMessage = do
  getMagicString
  size <- getWord32host
  code <- getWord32host
  payload <- getLazyByteString (fromIntegral size)

  case mkMessage code payload of
    Just msg -> return msg
    Nothing  -> fail $ "Unknown message type code: " <> show code

-- | Encode a binary sway IPC protocol message.
msgEncode :: Message -> ByteString
msgEncode = runPut . putMessage

-- | Decode a binary sway IPC protocol message.
msgDecode :: ByteString -> Either String Message
msgDecode bytes =
  case runGetOrFail getMessage bytes of
    Left  (_, _, err) -> Left  err
    Right (_, _, msg) -> Right msg

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
