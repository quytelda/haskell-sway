{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Description : A Rectangle type for describing graphical geometry.
-}
module System.Desktop.Sway.Rectangle where

import Data.Aeson

data Rectangle = Rectangle { rectX      :: Int
                           , rectY      :: Int
                           , rectWidth  :: Int
                           , rectHeight :: Int
                           } deriving (Eq, Show)

instance FromJSON Rectangle where
  parseJSON = withObject "Rectangle" $ \obj -> do
    rectX      <- obj .: "x"
    rectY      <- obj .: "y"
    rectWidth  <- obj .: "width"
    rectHeight <- obj .: "height"

    return Rectangle{..}
