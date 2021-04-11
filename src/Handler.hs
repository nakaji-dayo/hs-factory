{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
module Handler where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (ReaderT (ReaderT, runReaderT))
import           Data.IORef
import           GHC.Generics
import           Lib

type Context = IORef Int
newtype Handler a = Handler { unHandler :: ReaderT Context IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

data User = User
  { id    :: Int
  , name  :: String
  , email :: String
  } deriving (Show, Generic)

data Tweet = Tweet
  { id     :: Int
  , userId :: Int
  , body   :: String
  , postAt :: Int -- todo: use Time
  } deriving (Show, Generic)

runHandler (Handler h) = do
  r <- newIORef 0
  runReaderT h r

getTweets :: Int -> Handler [Tweet]
getTweets _userid = do
  liftIO $ putStrLn "getTweets called"
  pure [Tweet 0 0 "hello" 0]
