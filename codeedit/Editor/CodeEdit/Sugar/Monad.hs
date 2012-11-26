{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Editor.CodeEdit.Sugar.Monad
  ( Context (..)
  , SugarM(..), runSugarM
  , readContext, liftTransaction
  ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
import Editor.CodeEdit.Sugar.Types -- see export list
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Cache as Cache
import qualified Editor.Data.Infer as Infer

data Context = Context
  { scInferState :: Infer.RefMap
  , scConfig :: SugarConfig
  , scMContextHash :: Maybe Cache.KeyBS -- Nothing if converting pure expression
  }

newtype SugarM m a = SugarM (ReaderT Context (T m) a)
  deriving (Monad)

runSugarM :: Monad m => Context -> SugarM m a -> T m a
runSugarM ctx (SugarM action) = runReaderT action ctx

readContext :: Monad m => SugarM m Context
readContext = SugarM Reader.ask

liftTransaction :: Monad m => T m a -> SugarM m a
liftTransaction = SugarM . lift
