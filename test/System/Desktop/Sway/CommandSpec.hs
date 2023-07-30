{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.CommandSpec where

import Data.Aeson
import Data.Aeson.Types     (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Test.Hspec

import System.Desktop.Sway.Command

resultSuccess_json :: Value
resultSuccess_json = object [ "success" .= True ]

errorMessage :: String
errorMessage = "Unknown/invalid command 'should_fail'"

resultFailure_json :: Value
resultFailure_json = object [ "success"     .= False
                            , "parse_error" .= True
                            , "error"       .= errorMessage
                            ]

invalidResult_json :: Value
invalidResult_json = object [ "invalid" .= True ]

reply_RunCommand_success :: ByteString
reply_RunCommand_success = encode $ replicate 3 resultSuccess_json

reply_RunCommand_failure :: ByteString
reply_RunCommand_failure = encode [ resultSuccess_json
                                  , resultFailure_json
                                  , resultSuccess_json
                                  ]

specResult :: Spec
specResult = describe "status" $ do
  context "when parsing a well-formed response object" $ do
    it "should parse a success response" $
      parseMaybe status resultSuccess_json `shouldBe` Just (Right ())

    it "should parse a failure response" $
      let msg = "parse error: " <> errorMessage
      in parseMaybe status resultFailure_json `shouldBe` Just (Left msg)

  context "when parsing an invalid response object" $ do
    it "should fail" $
      parseMaybe status invalidResult_json `shouldBe` Nothing

spec :: Spec
spec = do
  specResult
