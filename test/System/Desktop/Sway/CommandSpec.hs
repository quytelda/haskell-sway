{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.CommandSpec where

import           Data.Aeson
import           Data.ByteString.Lazy        (ByteString)
import           Data.Either
import           Test.Hspec

import           System.Desktop.Sway.Command

resultSuccess_json :: Value
resultSuccess_json = object [ "success" .= True ]

errorMessage :: String
errorMessage = "Unknown/invalid command 'should_fail'"

resultFailure_json :: Value
resultFailure_json = object [ "success"     .= False
                            , "parse_error" .= True
                            , "error"       .= errorMessage
                            ]

reply_RunCommand_success :: ByteString
reply_RunCommand_success = encode $ replicate 3 resultSuccess_json

reply_RunCommand_failure :: ByteString
reply_RunCommand_failure = encode [ resultSuccess_json
                                  , resultFailure_json
                                  , resultSuccess_json
                                  ]

spec :: Spec
spec = undefined
