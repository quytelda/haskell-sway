{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module System.Desktop.Sway.TypesSpec where

import           Control.Monad.Except
import           Data.ByteString.Lazy      (ByteString)
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

instance SendRecv Mocket IO where
  recv = getMock
  send = putMock

returnUnit :: MonadError e m => SwayT s m ()
returnUnit = return ()

throwException :: (MonadError e m, FromString e) => SwayT s m ()
throwException = throwString "foo"

specSway :: Spec
specSway = describe "SwayT" $ do
  it "can return a wrapped value" $
    let result = runSwayT returnUnit () :: Either String ()
    in result `shouldBe` Right ()

  it "can return an exception value" $
    let result = runSwayT throwException () :: Either String ()
    in result `shouldBe` Left "foo"

  it "can send bytes" $ do
    mock <- newMock
    _ <- runSwayT (sendBytes "foobar") mock
    getMock mock `shouldReturn` "foobar"

  it "can receive bytes" $ do
    mock <- newMock
    putMock mock "foobar"
    runSwayT recvBytes mock `shouldReturn` "foobar"

