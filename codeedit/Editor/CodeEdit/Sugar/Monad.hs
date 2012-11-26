{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Editor.CodeEdit.Sugar.Monad
  ( Context(..), mkContext
  , SugarM(..), run, runPure
  , readContext, liftTransaction
  ) where

import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
import Editor.CodeEdit.Sugar.Infer (InferExpressionResult(..), ierContext, ierRefmap)
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

mkContext :: SugarConfig -> InferExpressionResult m -> Context
mkContext config iResult = Context
  { scInferState = iResult ^. ierRefmap
  , scConfig = config
  , scMContextHash = Just . Cache.bsOfKey $ iResult ^. ierContext
  }

run :: Monad m => Context -> SugarM m a -> T m a
run ctx (SugarM action) = runReaderT action ctx

runPure :: Monad m => SugarConfig -> SugarM m a -> T m a
runPure config =
  run ctx
  where
    ctx =
      Context
      { scInferState = error "pure expression doesnt have infer state"
      , scConfig = config
      , scMContextHash = Nothing
      }

readContext :: Monad m => SugarM m Context
readContext = SugarM Reader.ask

liftTransaction :: Monad m => T m a -> SugarM m a
liftTransaction = SugarM . lift
