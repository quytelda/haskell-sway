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

-- | The type of an input device.
--
-- This property appears in `Input` and `BindingEvent`, but neither
-- context makes use of all variants. Hence, the variants aren't
-- necessarily disjoint with regarding what devices they may
-- represent. For example, a physical mouse might be labelled either
-- `Mouse` or `Pointer` depending on context.
data InputType = Keyboard
               | Mouse
               | Pointer
               | Touch
               | TabletTool
               | TabletPad
               | Switch
               deriving (Eq, Show)

instance FromJSON InputType where
  parseJSON = withText "InputType" $ \text ->
    case text of
      "keyboard"    -> return Keyboard
      "mouse"       -> return Mouse
      "pointer"     -> return Pointer
      "touch"       -> return Touch
      "tablet_tool" -> return TabletTool
      "tablet_pad"  -> return TabletPad
      "switch"      -> return Switch
      _             -> fail $ "Unrecognized input type"

-- | A description of an input source.
data Input = Input { inputIdentifier      :: String
                   , inputName            :: String
                   , inputVendor          :: Int
                   , inputProduct         :: Int
                   , inputType            :: InputType
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

-- | An event generated when the binding mode changes.
data ModeEvent = ModeEvent { modeChange      :: String
                           , modePangoMarkup :: Bool
                           } deriving (Eq, Show)

instance FromJSON ModeEvent where
  parseJSON = withObject "ModeEvent" $ \obj -> do
    modeChange      <- obj .: "change"
    modePangoMarkup <- obj .: "pango_markup"

    return ModeEvent{..}

-- | An event generated whenever a binding is executed.
--
-- Currently, the only binding event type is "run".
data BindingEvent = BindingRun { bindingCommand        :: String
                               , bindingEventStateMask :: [String]
                               , bindingInputCode      :: Int
                               , bindingSymbol         :: String
                               , bindingInputType      :: InputType
                               } deriving (Eq, Show)

instance FromJSON BindingEvent where
  parseJSON = withObject "BindingEvent" $ \obj -> do
    binding               <- obj .: "binding"
    bindingCommand        <- binding .: "command"
    bindingEventStateMask <- binding .: "event_state_mask"
    bindingInputCode      <- binding .: "input_code"
    bindingSymbol         <- binding .: "symbol"
    bindingInputType      <- binding .: "input_type"

    return BindingRun{..}

-- | An event generated whenever an input device change occurs.
data InputEvent = InputAdded          Input
                | InputRemoved        Input
                | InputXKBKeymap      Input
                | InputXKBLayout      Input
                | InputLibinputConfig Input
                deriving (Eq, Show)

instance FromJSON InputEvent where
  parseJSON = withObject "InputEvent" $ \obj -> do
    change <- obj .: "change"
    let event = case change of
          "added"           -> return InputAdded
          "removed"         -> return InputRemoved
          "xkb_keymap"      -> return InputXKBKeymap
          "xkb_layout"      -> return InputXKBLayout
          "libinput_config" -> return InputLibinputConfig
          _                 -> fail $ "Unrecognized input event: " <> change

    event <*> obj .: "input"
