{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Input where

import           Control.Monad.Trans         (MonadIO)
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

getBindingModes :: (MonadIO m, SendRecv s) => SwayT s m [String]
getBindingModes = query GetBindingModes ""

getBindingState :: (MonadIO m, SendRecv s) => SwayT s m String
getBindingState = query GetBindingState "" >>= parseSway (.: "name")

data Input = Input { inputIdentifier           :: String
                   , inputName                 :: String
                   , inputVendor               :: Int
                   , inputProduct              :: Int
                   , inputType                 :: String
                   , inputXKBActiveLayoutName  :: Maybe String
                   , inputXKBLayoutNames       :: Maybe [String]
                   , inputXKBActiveLayoutIndex :: Maybe Int
                   , inputScrollFactor         :: Maybe Double
                   , inputLibinput             :: Maybe Object
                   } deriving (Eq, Show)

instance FromJSON Input where
  parseJSON = withObject "Input" $ \obj -> do
    inputIdentifier           <- obj .: "identifier"
    inputName                 <- obj .: "name"
    inputVendor               <- obj .: "vendor"
    inputProduct              <- obj .: "product"
    inputType                 <- obj .: "type"
    inputXKBActiveLayoutName  <- obj .:? "xkb_active_layout_name"
    inputXKBLayoutNames       <- obj .:? "xkb_layout_names"
    inputXKBActiveLayoutIndex <- obj .:? "xkb_active_layout_index"
    inputScrollFactor         <- obj .:? "scroll_factor"
    inputLibinput             <- obj .:? "libinput"

    return Input{..}

getInputs :: (MonadIO m, SendRecv s) => SwayT s m [Input]
getInputs = query GetInputs ""
