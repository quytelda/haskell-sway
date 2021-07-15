{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Command where

import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (except)
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import           Data.ByteString.Lazy       (ByteString)

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Types

-- | Parse an error message from a sway command failure result JSON object.
failure :: Object -> Parser String
failure obj = do
  parseError <- obj .: "parse_error"
  errorMsg   <- obj .: "error"
  return $ (parseError ? "parse error: ") <> errorMsg

-- | Parse a sway command result JSON object.
-- For success results, return unit.
-- For failure results, return an error message.
result :: Object -> Parser (Either String ())
result obj = do
  success <- obj .: "success"
  if success
    then Right <$> return ()
    else Left  <$> failure obj

-- | Run a sway command.
-- Send a RUN_COMMAND IPC message and return the reply payload.
runCommand :: (MonadIO m, SendRecv s) => ByteString -> SwayT s m ()
runCommand cmd = query RunCommand cmd
                 >>= parseSway results
                 >>= except . sequence_
  where
    results = withArray "results" $ mapM $ withObject "result" result
