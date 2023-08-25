{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Input where

import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.Exception
import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

getBindingModes :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [String]
getBindingModes = query GetBindingModes ""

getBindingState :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m String
getBindingState = query GetBindingState "" >>= parseSway (.: "name")

data Input = Input { inputIdentifier      :: String
                   , inputName            :: String
                   , inputVendor          :: Int
                   , inputProduct         :: Int
                   , inputType            :: String
                   , inputScrollFactor    :: Maybe Double
                   , inputLibinput        :: Maybe Object
                   , inputXKBLayoutNames  :: Maybe [String]
                   , inputXKBActiveLayout :: Maybe (Int, String)
                   } deriving (Eq, Show)

instance FromJSON Input where
  parseJSON = withObject "Input" $ \obj -> do
    inputIdentifier      <- obj .: "identifier"
    inputName            <- obj .: "name"
    inputVendor          <- obj .: "vendor"
    inputProduct         <- obj .: "product"
    inputType            <- obj .: "type"
    inputScrollFactor    <- obj .:? "scroll_factor"
    inputLibinput        <- obj .:? "libinput"

    inputXKBLayoutNames  <- obj .:? "xkb_layout_names"
    activeLayoutIndex    <- obj .:? "xkb_active_layout_index"
    activeLayoutName     <- obj .:? "xkb_active_layout_name"
    let inputXKBActiveLayout = liftM2 (,) activeLayoutIndex activeLayoutName

    return Input{..}

getInputs :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [Input]
getInputs = query GetInputs ""
