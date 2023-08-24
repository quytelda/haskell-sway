{-# LANGUAGE FlexibleInstances #-}

module System.Desktop.Sway.Exception where

import           Control.Monad.Except

{- See: https://stackoverflow.com/q/76963901/5129612 -}
class FromString a where
  fromString :: String -> a

instance FromString String where
  fromString = id

instance FromString IOError where
  fromString = userError

throwString :: (MonadError e m, FromString e) => String -> m a
throwString = throwError . fromString
