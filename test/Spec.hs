{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified System.Desktop.Sway.TypesSpec as TypesSpec
import qualified System.Desktop.Sway.CommandSpec as CommandSpec



main :: IO ()
main = hspec $ do
  describe "Types" TypesSpec.specMessage
  describe "Command" CommandSpec.spec
