{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.TypesSpec where

import           Control.Monad.Trans.Except (throwE)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.IORef
import           Test.Hspec

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Types

-- Example IPC message that sends the `exit` command.
-- Taken from the sway-ipc(7) man page.
msg_RunCommand_exit :: Message
msg_RunCommand_exit = Message RunCommand "exit"

bin_RunCommand_exit :: ByteString
bin_RunCommand_exit = BL.pack [ 0x69, 0x33, 0x2d, 0x69
                              , 0x70, 0x63, 0x04, 0x00
                              , 0x00, 0x00, 0x00, 0x00
                              , 0x00, 0x00, 0x65, 0x78
                              , 0x69, 0x74
                              ]

-- | Mocket is a mock socket for testing IPC socket operations.
newtype Mocket = Mocket (IORef ByteString)

newMock :: IO Mocket
newMock = Mocket <$> newIORef ""

putMock :: Mocket -> ByteString -> IO ()
putMock (Mocket ref) = writeIORef ref

getMock :: Mocket -> IO ByteString
getMock (Mocket ref) = readIORef ref

instance SendRecv Mocket where
  recv = getMock
  send = putMock

returnUnit :: Monad m => SwayT s m ()
returnUnit = return ()

throwException :: Monad m => SwayT s m ()
throwException = throwE "foo"

specSway :: Spec
specSway = describe "SwayT" $ do
  it "can return a wrapped value" $
    runSwayT returnUnit () `shouldReturn` Right ()

  it "can return an exception value" $
    runSwayT throwException () `shouldReturn` Left "foo"

  it "can send bytes" $ do
    mock <- newMock
    _ <- runSwayT (sendBytes "foobar") mock
    getMock mock `shouldReturn` "foobar"

  it "can receive bytes" $ do
    mock <- newMock
    putMock mock "foobar"
    runSwayT recvBytes mock `shouldReturn` Right "foobar"

specMessage :: Spec
specMessage = describe "Message" $ do
  specEncode
  specDecode

specEncode :: Spec
specEncode = describe "msgEncode" $ do
  it "can serialize messages" $
    msgEncode msg_RunCommand_exit `shouldBe` bin_RunCommand_exit

specDecode :: Spec
specDecode = describe "msgDecode" $ do
  it "can deserialize messages" $
    bin_RunCommand_exit `shouldYield` Right msg_RunCommand_exit

  it "expects a correct magic string" $
    "i3-xyz\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULnop" `shouldYield` Left "Expected magic string."

  it "expects a valid message type code" $
    "i3-ipc\ETX\NUL\NUL\NUL\SI\NUL\NUL\NULnop" `shouldYield` Left "Unknown message type code:15"

  it "expects enough bytes" $
    "i3-ipc\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL" `shouldYield` Left "not enough bytes"
  where
    shouldYield bs = shouldBe (msgDecode bs)
