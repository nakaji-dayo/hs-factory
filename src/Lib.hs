{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Lib where


import           Control.Monad.Indexed.State
import           Data.Default.Class
import           Data.Type.Map
import           GHC.Generics
import           GHC.TypeLits                (KnownSymbol)
import           Language.Haskell.DoNotation
import           Prelude                     hiding (Monad (..), pure)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Factory i j a = Factory { unFactory :: IxState i j a}

runFactory seeder f = do
  seeder
  f

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
  }
instance EntityW Tweet

-- seed = do
--   u <- new @User
--   t <- new @Tweet
--   pure ()

new :: (EntityW a,  Default a) => Var k -> IxState (Map m) (Map ((k ':-> a) : m)) a
new k = do
  let x = def
  imodify (Ext k x)
  pure x

runLoader = do
  let m = snd $ runIxState seed Empty
  sinsert m
  where
    seed = do
      new @User (Var @ "taro")
      new @User (Var @ "hanako")

class SeedS s where
  sinsert :: s -> IO ()

instance SeedS (Map '[]) where
  sinsert _ = print "end"
instance (EntityW v, SeedS (Map s)) => SeedS (Map ((k :-> v) ': s)) where
  sinsert (Ext k v s) = insert v >> sinsert s
