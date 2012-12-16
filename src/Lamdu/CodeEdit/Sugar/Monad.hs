{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.CodeEdit.Sugar.Monad
  ( Context(..), mkContext
  , SugarM(..), run, runPure
  , readContext, liftTransaction
  ) where

import Control.Applicative (Applicative)
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable)
import Lamdu.CodeEdit.Sugar.Config (SugarConfig)
import Lamdu.CodeEdit.Sugar.Infer (InferLoadedResult, ilrInferContext, ilrContext, ilrBaseInferContext)
import Lamdu.CodeEdit.Sugar.Types -- see export list
import Lamdu.Data.IRef (DefI)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Cache as Cache
import qualified Lamdu.Data.Infer as Infer

data Context t = Context
  { scInferState :: Infer.Context (DefI t)
  , scConfig :: SugarConfig t
  , scMContextHash :: Maybe Cache.KeyBS -- Nothing if converting pure expression
  , scHoleInferState :: Infer.Context (DefI t)
  }

newtype SugarM m a = SugarM (ReaderT (Context (Tag m)) (T m) a)
  deriving (Functor, Applicative, Monad)

mkContext :: Typeable (m ()) => SugarConfig (Tag m) -> InferLoadedResult m -> Context (Tag m)
mkContext config iResult = Context
  { scInferState = iResult ^. ilrInferContext
  , scConfig = config
  , scMContextHash = Just . Cache.bsOfKey $ iResult ^. ilrContext
  , scHoleInferState = iResult ^. ilrBaseInferContext
  }

run :: MonadA m => Context (Tag m) -> SugarM m a -> T m a
run ctx (SugarM action) = runReaderT action ctx

runPure :: MonadA m => SugarConfig (Tag m) -> SugarM m a -> T m a
runPure config =
  run ctx
  where
    ctx =
      Context
      { scInferState = error "pure expression doesnt have infer state"
      , scHoleInferState = error "pure expression doesnt have hole infer state"
      , scConfig = config
      , scMContextHash = Nothing
      }

readContext :: MonadA m => SugarM m (Context (Tag m))
readContext = SugarM Reader.ask

liftTransaction :: MonadA m => T m a -> SugarM m a
liftTransaction = SugarM . lift
