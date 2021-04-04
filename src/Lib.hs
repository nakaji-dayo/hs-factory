{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Lib where

import           Control.Exception           (assert)
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans (ilift)
import           Data.Default.Class
import           Data.Generics.Labels
import           Data.Kind                   (Constraint)
import           Data.Type.Map
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits                (KnownSymbol)
import           Language.Haskell.DoNotation
import           Lens.Micro
import           Prelude                     hiding (Monad (..), pure)
import qualified Prelude                     (Monad (..), pure)

class EntityW a where
  insert :: a -> IO ()

new ::  forall k a m f.(Default a, Factory f) => (a -> a) -> IxStateT f (Map m) (Map ((k ':-> a) : m)) a
new m = do
  let x = m def
  imodify (Ext (Var :: Var k) x)
  pure x

-- new' :: forall k a m. (Default a) => IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
-- new' = new Prelude.id

get :: forall k a m f. (IsMember k a m, Factory f) => IxStateT f (Map m) (Map m) a
get =
  lookp (Var :: Var k) <$> iget

update :: (IsMember k a m) => Var k -> (a -> a) -> IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
update k m = do
  x <- m . lookp k <$> iget
  imodify (Ext k x)
  pure x

class Monad m => Factory m where
  type Entity m :: * -> Constraint
  insertM :: Entity m a => a -> m ()

type FactoryW m i j a = IxStateT m i j a

withFactory :: (SeedS m i, Factory m)
  => IxStateT m (Map '[]) i a -> IxStateT m i j b -> m (b, j)
withFactory seed f =
  runIxStateT g Empty
  where
    g = do
      seed
      m <- iget
      ilift $ sinsert m
      f

-- internal, UndecidableInstances
class SeedS m s where
  sinsert :: (Factory m, Applicative m) => s -> m ()
instance SeedS m (Map '[]) where
  sinsert _ = pure ()
instance (SeedS m (Map s), Entity m v) => SeedS m (Map ((k :-> v) ': s)) where
  sinsert (Ext k v s) =
    insertM v Prelude.>> sinsert s

type MySeed i j = IxStateT IO (Map i) (Map j) ()
