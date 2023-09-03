{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Types and functions for running sway commands.
-}
module System.Desktop.Sway.Command where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types            (Parser, listParser)
import           Data.ByteString.Lazy        (ByteString)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Message
import           System.Desktop.Sway.Types

-- | Parse a sway command result JSON object.
-- For success results, return unit.
-- For failure results, return an error message.
status :: Value -> Parser (Either String ())
status = withObject "status" $ \obj -> do
  success <- obj .: "success"
  if success
    then Right <$> return ()
    else Left  <$> do
    parseError <- obj .: "parse_error"
    errorMsg   <- obj .: "error"
    return $ (guard parseError >> "parse error: ") <> errorMsg

-- | Run a sway command.
-- Send a RUN_COMMAND IPC message and return the reply payload.
runCommand :: (MonadError e m, FromString e, SendRecv s m) => ByteString -> SwayT s m ()
runCommand cmd = query RUN_COMMAND cmd
                 >>= parseSway results
                 >>= eitherToSway . sequence_
  where
    results = listParser status
