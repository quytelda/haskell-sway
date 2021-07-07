{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.IORef
import           Test.Hspec

import           System.Desktop.Sway.Types

commandExitMsg = Message RunCommand "exit"
commandExitBin = BL.pack [ 0x69, 0x33, 0x2d, 0x69
                         , 0x70, 0x63, 0x04, 0x00
                         , 0x00, 0x00, 0x00, 0x00
                         , 0x00, 0x00, 0x65, 0x78
                         , 0x69, 0x74
                         ]

failureParse :: ByteString
failureParse = "Unknown/invalid command 'wrong'"

resultSuccess = "{ \"success\": true }"
resultFailure = mconcat [ "{ \"success\": false"
                        , ", \"parse_error\": true"
                        , ", \"error\": \"" <> failureParse <> "\" "
                        , "}"
                        ]

reply_RunCommand_1 :: ByteString
reply_RunCommand_1 = "[ " <> resultSuccess <> " ]"

reply_RunCommand_2 :: ByteString
reply_RunCommand_2 = mconcat [ "[ " <> resultSuccess
                             , ", " <> resultFailure <> " "
                             , "]"
                             ]

-- | Mocket is a mock socket for testing IPC socket operations.
newtype Mocket = Mocket (IORef ByteString)

instance SendRecv Mocket where
  recv (Mocket m) = readIORef m
  send (Mocket m) = writeIORef m

main :: IO ()
main = hspec $ do
  describe "Message" $ do
    it "can encode Messages to binary" $
      msgEncode commandExitMsg `shouldBe` commandExitBin

    it "can decode Messages from binary" $
      msgDecode commandExitBin `shouldBe` Right commandExitMsg

  describe "parseResults" $ do
    it "should succeed if every command succeeded" $
      parseResults reply_RunCommand_1 `shouldBe` Right ()
    it "should fail if any command failed" $
      let err = "Error in $: parse error: " <> Char8.unpack failureParse
      in parseResults reply_RunCommand_2 `shouldBe` Left err

  describe "SwayT" $ do
    it "can send raw bytes" $ do
      ref <- newIORef ""
      runSwayT (sendBytes commandExitBin) (Mocket ref)
      readIORef ref `shouldReturn` commandExitBin

    it "can receive raw bytes" $ do
      ref <- newIORef commandExitBin
      runSwayT recvBytes (Mocket ref) `shouldReturn` Right commandExitBin
