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
module Lib where

import           Control.Exception           (assert)
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans (ilift)
import           Control.Monad.Trans.Ix      (liftIx)
import           Data.Default.Class
import           Data.Generics.Labels
import           Data.Kind                   (Constraint)
import           Data.Type.Map
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits                (AppendSymbol, KnownSymbol,
                                              SomeSymbol, Symbol)
import           Language.Haskell.DoNotation
import           Lens.Micro
import           Prelude                     hiding (Monad (..), pure)
import qualified Prelude                     as P (Monad (..), pure)
import Control.Monad (forM_, void, forM)
import Data.Bifunctor (bimap, first, Bifunctor (second))

type IxSeed m s s' a = IxStateT m (Map s, [EntityW m]) (Map s', [EntityW m]) a
type AddSeed m s k v a = IxSeed m s ((k ':-> v) : s)  a

new ::  forall k a s m. (Default a, Factory m, Entity m a) => (a -> a) -> AddSeed m s k a a
new m = do
  let x = m def
  imodify $ bimap (Ext (Var :: Var k) x) (EntityW x :)
  pure x

new_ ::  forall a s m. (Default a, Factory m, Entity m a) => (a -> a) -> IxSeed m s s a
new_ m = do
  let x = m def
  imodify $ second (EntityW x :)
  pure x

-- new' :: forall k a m. (Default a) => IxStateT IO (Map m) (Map ((k ':-> a) : m)) a
-- new' = new Prelude.id

get :: forall k a m f. (IsMember k a m, Factory f) => IxSeed f m m a
get =
  lookp (Var :: Var k) . fst <$> iget

update :: forall k a s m. (IsMember k a s, Updatable k a s s, Factory m)
  => (a -> a) -> IxSeed m s s a
update m = do
  x <- m . lookp (Var :: Var k) . fst <$> iget
  imodify $ first (\ s -> Data.Type.Map.update s (Var :: Var k) x)
  pure x

class Monad m => Factory m where
  type Entity m :: * -> Constraint
  insertM :: Entity m a => a -> m ()
  cleanup :: m ()

data EntityW m = forall a. (Entity m a) => EntityW a

withFactory :: (Factory m)
   => IxStateT m (Map '[], [EntityW m]) (Map i, [EntityW m]) a -> IxStateT m (Map i, [EntityW m]) j b -> m ()
withFactory seed f = do
  runIxStateT g (Empty, [])
  cleanup
  where
    g = do
      seed
      xs <- igets snd
      ilift . mapM_ insert' $ reverse xs
      f
    insert' :: Factory m => EntityW m -> m ()
    insert' (EntityW x) = insertM x
