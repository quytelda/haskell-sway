module System.Desktop.Sway.Types where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

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
                 deriving (Show, Eq)

data EventType = Workspace
               | Mode
               | Window
               | BarConfigUpdate
               | Binding
               | Shutdown
               | Tick
               | BarStateUpdate
               | Input
               deriving (Show)

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
  [ (Workspace,       0x80000000)
  , (Mode,            0x80000002)
  , (Window,          0x80000003)
  , (BarConfigUpdate, 0x80000004)
  , (Binding,         0x80000005)
  , (Shutdown,        0x80000006)
  , (Tick,            0x80000007)
  , (BarStateUpdate,  0x80000014)
  , (Input,           0x80000015)
  ]

data Message = Message MessageType ByteString
             | Event   EventType   ByteString
             deriving (Show)

msgData :: Message -> ByteString
msgData (Message _ bytes) = bytes
msgData (Event   _ bytes) = bytes
