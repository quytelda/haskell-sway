{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : IPC functionality related to output devices.
-}
module System.Desktop.Sway.Output where

import           Control.Monad.Except
import           Data.Aeson

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Rectangle
import           System.Desktop.Sway.Types

-- | A representation of a monitor output mode.
data OutputMode = OutputMode { modeWidth   :: Int
                             , modeHeight  :: Int
                             , modeRefresh :: Int
                             } deriving (Eq, Show)

instance FromJSON OutputMode where
  parseJSON = withObject "OutputMode" $ \obj -> do
    modeWidth   <- obj .: "width"
    modeHeight  <- obj .: "height"
    modeRefresh <- obj .: "refresh"

    return OutputMode{..}

-- | A description of a graphical output device.
data Output = Output { outputName             ::  String
                     , outputMake             ::  String
                     , outputModel            ::  String
                     , outputSerial           ::  String
                     , outputActive           ::  Bool
                     , outputDPMS             ::  Bool
                     , outputPower            ::  Bool
                     , outputPrimary          ::  Bool
                     , outputScale            ::  Double
                     , outputSubpixelHinting  ::  String
                     , outputTransform        ::  String
                     , outputCurrentWorkspace ::  String
                     , outputModes            ::  [OutputMode]
                     , outputCurrentMode      ::  OutputMode
                     , outputRect             ::  Rectangle
                     } deriving (Eq, Show)

instance FromJSON Output where
  parseJSON = withObject "Output" $ \obj -> do
    outputName             <- obj .: "name"
    outputMake             <- obj .: "make"
    outputModel            <- obj .: "model"
    outputSerial           <- obj .: "serial"
    outputActive           <- obj .: "active"
    outputDPMS             <- obj .: "dpms"
    outputPower            <- obj .: "power"
    outputPrimary          <- obj .: "primary"
    outputScale            <- obj .: "scale"
    outputSubpixelHinting  <- obj .: "subpixel_hinting"
    outputTransform        <- obj .: "transform"
    outputCurrentWorkspace <- obj .: "current_workspace"
    outputModes            <- obj .: "modes"
    outputCurrentMode      <- obj .: "current_mode"
    outputRect             <- obj .: "rect"

    return Output{..}

-- | Get the list of sway outputs.
-- Send a `GET_OUTPUTS` IPC message and return the parsed results.
getOutputs :: (MonadError e m, FromString e, SendRecv s m) => SwayT s m [Output]
getOutputs = query GET_OUTPUTS ""
