{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Lib where


import           Control.Exception           (assert)
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans (ilift)
import           Data.Default.Class
import           Data.Generics.Labels
import           Data.Type.Map
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits                (KnownSymbol)
import           Language.Haskell.DoNotation
import           Lens.Micro
import           Prelude                     hiding (Monad (..), pure)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Factory i j a = Factory { unFactory :: IxState i j a}

class EntityW a where
  insert :: a -> IO ()
  bulkInsert :: [a] -> IO ()
  bulkInsert = mapM_ insert

class FactoryW a where
  clean :: a -> IO ()

data User = User
  { id   :: Int
  , name :: String
  , edge :: Int
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

new :: (EntityW a,  Default a) => Var k -> (a -> a) -> IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
new k m = do
  let x = m def
  imodify (Ext k x)
  pure x

new' :: (EntityW a,  Default a) => Var k -> IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
new' k = new k Prelude.id

get :: (EntityW a, IsMember k a m) => Var k -> IxStateT IO (Map m) (Map m) a
get k =
  lookp k <$> iget

runFactory seed = do
  let m = snd $ runIxState seed Empty
  sinsert m

withFactory seed f =
  runIxStateT g Empty
  where
    g = do
      seed
      m <- iget
      ilift $ sinsert m
      f

myseed = do
  t <- new' @User (Var @ "taro")
  new' @User (Var @ "hanako")
  new @Tweet (Var @ "t1") $ #userId .~ (t ^. #id)
  pure ()

mytest = withFactory myseed $ do
  t <- get @User (Var @ "taro")
  ts <- ilift $ mylogic (t ^. #id)
  get @User (Var @ "taro")
  assert (length ts == 1) (pure ())
  where
    mylogic :: Int -> IO [Tweet]
    mylogic _userid =
      pure [Tweet 0 0 "hello" 0]

class SeedS s where
  sinsert :: s -> IO ()

instance SeedS (Map '[]) where
  sinsert _ = print "end"
instance (EntityW v, SeedS (Map s)) => SeedS (Map ((k :-> v) ': s)) where
  sinsert (Ext k v s) = insert v >> sinsert s
