{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Types where

import           Control.Applicative
import           Control.Monad        (when)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.List            (find)
import           Data.Maybe           (fromJust)
import           Data.Word

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

data EventType = Workspace
               | Mode
               | Window
               | BarConfigUpdate
               | Binding
               | Shutdown
               | Tick
               | BarStateUpdate
               | Input
               deriving (Eq, Show)

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
    Nothing  -> fail $ "Unknown message type code:" <> show code

-- | Encode a binary sway IPC protocol message.
msgEncode :: Message -> ByteString
msgEncode = runPut . putMessage

-- | Decode a binary sway IPC protocol message.
msgDecode :: ByteString -> Either String Message
msgDecode bytes =
  case runGetOrFail getMessage bytes of
    Left  (_, _, err) -> Left  err
    Right (_, _, msg) -> Right msg
