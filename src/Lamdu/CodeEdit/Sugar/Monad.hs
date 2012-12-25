{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.CodeEdit.Sugar.Monad
  ( Context(..), mkContext
  , InferParams(..)
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
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Cache as Cache
import qualified Lamdu.Data.Expression.Infer as Infer

data InferParams t = InferParams
  { ipMDefI :: Maybe (DefI t)
  , ipLoader :: Infer.Loader (DefI t) Maybe
  }

data Context t = Context
  { scInferParams :: InferParams t
  , scInferState :: Infer.Context (DefI t)
  , scConfig :: SugarConfig t
  , scMContextHash :: Maybe Cache.KeyBS -- Nothing if converting pure expression
  , scHoleInferState :: Infer.Context (DefI t)
  }

newtype SugarM m a = SugarM (ReaderT (Context (Tag m)) (T m) a)
  deriving (Functor, Applicative, Monad)

mkContext ::
  Typeable (m ()) =>
  Maybe (DefI (Tag m)) -> SugarConfig (Tag m) -> InferLoadedResult m -> Context (Tag m)
mkContext mDefI config iResult = Context
  { scInferParams =
    InferParams
    { ipMDefI = mDefI
    , ipLoader = Infer.loaderOfExisting $ iResult ^. ilrContext
    }
  , scInferState = iResult ^. ilrInferContext
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
      { scInferParams = error "pure expression doesn't have infer params"
      , scInferState = error "pure expression doesn't have infer state"
      , scHoleInferState = error "pure expression doesn't have hole infer state"
      , scConfig = config
      , scMContextHash = Nothing
      }

readContext :: MonadA m => SugarM m (Context (Tag m))
readContext = SugarM Reader.ask

liftTransaction :: MonadA m => T m a -> SugarM m a
liftTransaction = SugarM . lift
