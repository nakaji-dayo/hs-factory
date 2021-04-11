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
                                              MonadReader (ask),
                                              ReaderT (ReaderT, runReaderT))
import           Data.Default.Class
import           Data.IORef
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
  insertM x = do
    liftIO $ putStrLn $ "INSERT " <> show x
  cleanup = liftIO $ putStrLn "TRUNCATE ALL TABLES"

instance Default User
instance Default Tweet

-- idの生成どうする
--   clientで生成できるケースとseqのケースとかある。両方サポートしたい
--   -- preinsert, postinsertで
--      idの生成、生成したidでのmodofyをする <- よさそう
-- myseed :: MySeed m n
myseed = do
  t <- new @"taro" @User $ Prelude.id
  new @"hanako" @User Prelude.id
  new @"t1" @Tweet $ #userId .~ (t ^. #id)
  new_ @Tweet $ #body .~ "foo"
  new_ @Tweet $ #body .~ "bar"

mytest = runHandler $ withFactory myseed $ do
  t <- get @"taro" @User
  ts <- ilift $ getTweets (t ^. #id)
  assert (length ts == 1) (pure ())
  assert (ts ^? ix 0 . #userId == Just (t ^. #id)) (pure ())

myseed' = do
  myseed
  update @"hanako" @User $ #email .~ "new@example.com"

mytest' = runHandler $ withFactory myseed' $ pure ()


testt :: Factory m => EntityW m -> m ()
testt e =
  case e of
    EntityW x -> insertM x
