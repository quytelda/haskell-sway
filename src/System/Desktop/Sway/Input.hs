{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Desktop.Sway.Input where

import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

getBindingModes :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [String]
getBindingModes = query GET_BINDING_MODES ""

getBindingState :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m String
getBindingState = query GET_BINDING_STATE "" >>= parseSway (.: "name")

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
getInputs = query GET_INPUTS ""

data Seat = Seat { seatName         :: String
                 , seatCapabilities :: Int
                 , seatFocus        :: Int
                 , seatDevices      :: [Input]
                 } deriving (Eq, Show)

instance FromJSON Seat where
  parseJSON = withObject "Seat" $ \obj -> do
    seatName         <- obj .: "name"
    seatCapabilities <- obj .: "capabilities"
    seatFocus        <- obj .: "focus"
    seatDevices      <- obj .: "devices"

    return Seat{..}

getSeats :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [Seat]
getSeats = query GET_SEATS ""
