{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : Types and functions for encoding and decoding sway IPC messages.
-}
module System.Desktop.Sway.Message where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (find)
import           Data.Maybe
import           Data.Word

data MessageType = RUN_COMMAND
                 | GET_WORKSPACES
                 | SUBSCRIBE
                 | GET_OUTPUTS
                 | GET_TREE
                 | GET_MARKS
                 | GET_BAR_CONFIG
                 | GET_VERSION
                 | GET_BINDING_MODES
                 | GET_CONFIG
                 | SEND_TICK
                 | SYNC
                 | GET_BINDING_STATE
                 | GET_INPUTS
                 | GET_SEATS
                 deriving (Eq, Show)

data EventType   = WORKSPACE
                 | MODE
                 | WINDOW
                 | BAR_CONFIG_UPDATE
                 | BINDING
                 | SHUTDOWN
                 | TICK
                 | BAR_STATE_UPDATE
                 | INPUT
                 deriving (Eq, Show)

instance ToJSON EventType where
  toJSON WORKSPACE         = "workspace"
  toJSON MODE              = "mode"
  toJSON WINDOW            = "window"
  toJSON BAR_CONFIG_UPDATE = "barconfig_update"
  toJSON BINDING           = "binding"
  toJSON SHUTDOWN          = "shutdown"
  toJSON TICK              = "tick"
  toJSON BAR_STATE_UPDATE  = "bar_state_update"
  toJSON INPUT             = "input"

msgCodes :: [(MessageType, Word32)]
msgCodes =
  [ (RUN_COMMAND,       0x00000000)
  , (GET_WORKSPACES,    0x00000001)
  , (SUBSCRIBE,         0x00000002)
  , (GET_OUTPUTS,       0x00000003)
  , (GET_TREE,          0x00000004)
  , (GET_MARKS,         0x00000005)
  , (GET_BAR_CONFIG,    0x00000006)
  , (GET_VERSION,       0x00000007)
  , (GET_BINDING_MODES, 0x00000008)
  , (GET_CONFIG,        0x00000009)
  , (SEND_TICK,         0x0000000a)
  , (SYNC,              0x0000000b)
  , (GET_BINDING_STATE, 0x0000000c)
  , (GET_INPUTS,        0x00000064)
  , (GET_SEATS,         0x00000065)
  ]

evtCodes :: [(EventType, Word32)]
evtCodes =
  [ (WORKSPACE,         0x80000000)
  , (MODE,              0x80000002)
  , (WINDOW,            0x80000003)
  , (BAR_CONFIG_UPDATE, 0x80000004)
  , (BINDING,           0x80000005)
  , (SHUTDOWN,          0x80000006)
  , (TICK,              0x80000007)
  , (BAR_STATE_UPDATE,  0x80000014)
  , (INPUT,             0x80000015)
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
msgLength = fromIntegral . BSL.length . msgPayload

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
  bytes <- getLazyByteString (BSL.length magicString)
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
