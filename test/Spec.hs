{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified System.Desktop.Sway.CommandSpec as CommandSpec
import qualified System.Desktop.Sway.MessageSpec as MessageSpec
import qualified System.Desktop.Sway.TypesSpec   as TypesSpec

main :: IO ()
main = hspec $ do
  describe "Types" TypesSpec.specSway
  describe "Command" CommandSpec.spec
  describe "Message" MessageSpec.spec
