{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
module Example where

import           Control.Exception
import           Control.Monad.Indexed.Trans
import           Data.Default.Class
import           Data.Type.Map               (Var (Var))
import           GHC.Generics                (Generic)
import           GHC.OverloadedLabels
import           Language.Haskell.DoNotation
import           Lens.Micro
import           Lib
import           Prelude                     hiding (Monad (..), pure)

data User = User
  { id    :: Int
  , name  :: String
  , email :: String
  } deriving (Show, Generic)
instance EntityW User where
  insert x = print $ "INSERT INTO USER VALUES " <> show x
instance Default User

data Tweet = Tweet
  { id     :: Int
  , userId :: Int
  , body   :: String
  , postAt :: Int -- todo: use Time
  } deriving (Show, Generic)
instance EntityW Tweet where
  insert x = print $ "INSERT INTO TWEET VALUES " <> show x
instance Default Tweet

myseed = do
  t <- new' @"taro" @User
  new' @"hanako" @User
  new @"t1" @Tweet  $ #userId .~ (t ^. #id)
  pure ()

mytest = withFactory myseed $ do
  t <- get @"taro" @User
  ts <- ilift $ getTweets (t ^. #id)
  assert (length ts == 1) (pure ())
  assert (ts ^? ix 0 . #userId == Just (t ^. #id)) (pure ())

getTweets :: Int -> IO [Tweet]
getTweets _userid =
  pure [Tweet 0 0 "hello" 0]

-- myseed' = do
--   myseed
--   update @User (Var @ "hanako") $ #email .~ "new@example.com"

-- mytest' = withFactory myseed' $ pure ()
