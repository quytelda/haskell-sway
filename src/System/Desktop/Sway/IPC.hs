{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.IPC where

import           Control.Monad
import           Network.Socket
import           System.Environment (lookupEnv)

getSocketAddr :: IO (Maybe SockAddr)
getSocketAddr = liftM SockAddrUnix <$> lookupEnv "SWAYSOCK"
