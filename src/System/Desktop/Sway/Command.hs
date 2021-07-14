{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.Command where

import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (except, throwE)
import           Data.Aeson
import           Data.Aeson.Types           (Parser, parseEither)
import           Data.ByteString.Lazy       (ByteString)

import           System.Desktop.Sway.Types
import           System.Desktop.Sway.IPC

-- | Parse an error message from a sway command failure result JSON object.
parseFailure :: Object -> Parser String
parseFailure obj = do
  parseError <- obj .: "parse_error"
  errorMsg   <- obj .: "error"
  return $ (parseError ? "parse error: ") <> errorMsg

-- | Parse a sway command result JSON object.
-- For success results, return unit.
-- For failure results, return an error message.
parseResult :: Value -> Parser (Either String ())
parseResult = withObject "command result" $ \obj -> do
  success <- obj .: "success"
  if success
    then Right <$> return ()
    else Left  <$> parseFailure obj

-- | Parse a `RUN_COMMAND` reply payload, which is an array of objects
-- indicating the respective success or failure of each command sent.
decodeResults :: ByteString -> Either String ()
decodeResults bytes = eitherDecode bytes
            >>= parseEither (mapArray parseResult)
            >>= sequence_
  where
    mapArray = withArray "Array" . mapM

-- | Run a sway command.
-- Send a RUN_COMMAND IPC message and return the reply payload.
runCommand :: (MonadIO m, SendRecv s) => ByteString -> SwayT s m ()
runCommand cmd = do
  reply <- ipc $ Message RunCommand cmd
  case reply of
    Message RunCommand payload -> except $ decodeResults payload
    _                          -> throwE "expected RUN_COMMAND reply"

