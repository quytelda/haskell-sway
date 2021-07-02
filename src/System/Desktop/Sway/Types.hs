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

data Message = Message MessageType ByteString
             | Event   EventType   ByteString
             deriving (Show)

msgData :: Message -> ByteString
msgData (Message _ bytes) = bytes
msgData (Event   _ bytes) = bytes
