{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BL
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

  describe "SwayT" $ do
    it "can send raw bytes" $ do
      ref <- newIORef ""
      runSwayT (sendBytes commandExitBin) (Mocket ref)
      readIORef ref `shouldReturn` commandExitBin

    it "can receive raw bytes" $ do
      ref <- newIORef commandExitBin
      runSwayT recvBytes (Mocket ref) `shouldReturn` Right commandExitBin
