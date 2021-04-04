{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Example where

import           Control.Exception           (assert)
import           Control.Monad.Indexed.Trans
import           Control.Monad.Reader        (MonadIO (liftIO),
                                              ReaderT (ReaderT, runReaderT))
import           Data.Default.Class
import           Data.Type.Map               (Var (Var))
import           GHC.Generics                (Generic)
import           GHC.OverloadedLabels
import           Handler
import           Language.Haskell.DoNotation
import           Lens.Micro
import           Lib
import           Prelude                     hiding (Monad (..), pure)
import qualified Prelude                     (Monad (..))

-- data MyFactory
-- instance FactoryW MyFactory where
--   insertW :: Show x => x -> IO ()
--   insertW = undefined
--   clean = undefined



instance Factory Handler where
  type Entity Handler = Show
  insertM x = liftIO $ putStrLn $ "INSERT " <> show x

instance Default User
instance Default Tweet

-- myseed :: MySeed m n
myseed = do
  t <- new @"taro" @User Prelude.id
  new @"hanako" @User Prelude.id
  new @"t1" @Tweet  $ #userId .~ (t ^. #id)
  -- ilift $ print "hello"
  pure ()

mytest = runHandler $ withFactory myseed $ do
  t <- get @"taro" @User
  ts <- ilift $ getTweets (t ^. #id)
  assert (length ts == 1) (pure ())
  assert (ts ^? ix 0 . #userId == Just (t ^. #id)) (pure ())

-- myseed' = do
--   myseed
--   update @User (Var @ "hanako") $ #email .~ "new@example.com"

-- mytest' = withFactory myseed' $ pure ()
