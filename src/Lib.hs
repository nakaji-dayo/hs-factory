{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

newtype Factory i j a = Factory { unFactory :: IxState i j a}

class EntityW a where
  insert :: a -> IO ()
  bulkInsert :: [a] -> IO ()
  bulkInsert = mapM_ insert

class FactoryW a where
  clean :: a -> IO ()


new ::  forall k a m.(EntityW a, Default a) => (a -> a) -> IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
new m = do
  let x = m def
  imodify (Ext (Var :: Var k) x)
  pure x

new' :: forall k a m. (EntityW a,  Default a) => IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
new' = new Prelude.id

get :: forall k a m. (IsMember k a m) => IxStateT IO (Map m) (Map m) a
get =
  lookp (Var :: Var k) <$> iget

update :: (EntityW a, IsMember k a m) => Var k -> (a -> a) -> IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
update k m = do
  x <- m . lookp k <$> iget
  imodify (Ext k x)
  pure x

withFactory :: SeedS i0 => IxStateT IO (Map '[]) i0 a -> IxStateT IO i0 k2 a1 -> IO (a1, k2)
withFactory seed f =
  runIxStateT g Empty
  where
    g = do
      seed
      m <- iget
      ilift $ sinsert m
      f

class SeedS s where
  sinsert :: s -> IO ()
instance SeedS (Map '[]) where
  sinsert _ = print "end"
instance (EntityW v, SeedS (Map s)) => SeedS (Map ((k :-> v) ': s)) where
  sinsert (Ext k v s) = insert v >> sinsert s
