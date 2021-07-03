{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy      as BL
import           Test.Hspec

import           System.Desktop.Sway.Types

commandExitMsg = Message RunCommand "exit"
commandExitBin = BL.pack [ 0x69, 0x33, 0x2d, 0x69
                         , 0x70, 0x63, 0x04, 0x00
                         , 0x00, 0x00, 0x00, 0x00
                         , 0x00, 0x00, 0x65, 0x78
                         , 0x69, 0x74
                         ]

main :: IO ()
main = hspec $ do
  describe "Message" $ do
    it "can encode Messages to binary" $
      runPut (putMessage commandExitMsg) `shouldBe` commandExitBin

    it "can decode Messages from binary" $
      runGet getMessage commandExitBin `shouldBe` commandExitMsg
