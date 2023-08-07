{-# LANGUAGE OverloadedStrings #-}

module System.Desktop.Sway.TypesSpec where

import           Control.Monad.Trans.Except (throwE)
import           Data.ByteString.Lazy       (ByteString)
import           Data.IORef
import           Test.Hspec

import           System.Desktop.Sway.IPC
import           System.Desktop.Sway.Types

-- | Mocket is a mock socket for testing IPC socket operations.
newtype Mocket = Mocket (IORef ByteString)

newMock :: IO Mocket
newMock = Mocket <$> newIORef ""

putMock :: Mocket -> ByteString -> IO ()
putMock (Mocket ref) = writeIORef ref

getMock :: Mocket -> IO ByteString
getMock (Mocket ref) = readIORef ref

instance SendRecv Mocket where
  recv = getMock
  send = putMock

returnUnit :: Monad m => SwayT s m ()
returnUnit = return ()

throwException :: Monad m => SwayT s m ()
throwException = throwE "foo"

specSway :: Spec
specSway = describe "SwayT" $ do
  it "can return a wrapped value" $
    runSwayT returnUnit () `shouldReturn` Right ()

  it "can return an exception value" $
    runSwayT throwException () `shouldReturn` Left "foo"

  it "can send bytes" $ do
    mock <- newMock
    _ <- runSwayT (sendBytes "foobar") mock
    getMock mock `shouldReturn` "foobar"

  it "can receive bytes" $ do
    mock <- newMock
    putMock mock "foobar"
    runSwayT recvBytes mock `shouldReturn` Right "foobar"

