{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.MessageSpec where

import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BL
import           Test.Hspec

import           System.Desktop.Sway.Message

-- Example IPC message that sends the `exit` command.
-- Taken from the sway-ipc(7) man page.
msg_RunCommand_exit :: Message
msg_RunCommand_exit = Message RUN_COMMAND "exit"

bin_RunCommand_exit :: ByteString
bin_RunCommand_exit = BL.pack [ 0x69, 0x33, 0x2d, 0x69
                              , 0x70, 0x63, 0x04, 0x00
                              , 0x00, 0x00, 0x00, 0x00
                              , 0x00, 0x00, 0x65, 0x78
                              , 0x69, 0x74
                              ]

spec :: Spec
spec = do
  specEncode
  specDecode

specEncode :: Spec
specEncode = describe "msgEncode" $ do
  it "can encode binary messages" $
    msgEncode msg_RunCommand_exit `shouldBe` bin_RunCommand_exit

specDecode :: Spec
specDecode = describe "msgDecode" $ do
  it "can decode binary messages" $
    bin_RunCommand_exit `shouldYield` Right msg_RunCommand_exit

  it "expects a correct magic string" $
    "i3-xyz\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULnop" `shouldYield` Left "Expected magic string."

  it "expects a valid message type code" $
    "i3-ipc\ETX\NUL\NUL\NUL\SI\NUL\NUL\NULnop" `shouldYield` Left "Unknown message type code: 15"

  it "expects enough input bytes" $
    "i3-ipc\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL" `shouldYield` Left "not enough bytes"
  where
    shouldYield bs = shouldBe (msgDecode bs)
