{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.Monad
  ( Context(..), TagParamInfo(..), RecordParamsInfo(..)
  , scMDefI, scInferState, scMContextHash, scHoleInferState
  , scCodeAnchors, scSpecialFunctions, scMReinferRoot, scTagParamInfos, scRecordParamsInfos
  , mkContext
  , SugarM(..), run, runPure
  , readContext, liftTransaction, local
  , codeAnchor
  , getP
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable)
import Lamdu.CodeEdit.Sugar.Infer (InferLoadedResult, ilrInferContext, ilrContext, ilrBaseInferContext)
import Lamdu.CodeEdit.Sugar.Types -- see export list
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression.Infer as Infer

data TagParamInfo = TagParamInfo
  { tpiFromParameters :: Guid
  , tpiJumpTo :: Guid
  }

newtype RecordParamsInfo = RecordParamsInfo
  { rpiFromDefinition :: Guid
  }

data Context m = Context
  { _scMDefI :: Maybe (DefI (Tag m))
  , _scInferState :: Infer.Context (DefI (Tag m))
  , _scMContextHash :: Maybe Cache.KeyBS -- Nothing if converting pure expression
  , _scHoleInferState :: Infer.Context (DefI (Tag m))
  , _scCodeAnchors :: Anchors.CodeProps m
  , _scSpecialFunctions :: Anchors.SpecialFunctions (Tag m)
  , _scMReinferRoot :: Maybe (String -> CT m Bool)
  , _scTagParamInfos :: Map Guid TagParamInfo -- tag guids
  , _scRecordParamsInfos :: Map Guid RecordParamsInfo -- param guids
  }
LensTH.makeLenses ''Context

newtype SugarM m a = SugarM (ReaderT (Context m) (T m) a)
  deriving (Functor, Applicative, Monad)

mkContext ::
  MonadA m => Typeable (m ()) =>
  Anchors.CodeProps m ->
  Maybe (DefI (Tag m)) ->
  Maybe (String -> CT m Bool) ->
  InferLoadedResult m ->
  T m (Context m)
mkContext cp mDefI mReinferRoot iResult = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scMDefI = mDefI
    , _scInferState = iResult ^. ilrInferContext
    , _scMContextHash = Just . Cache.bsOfKey $ iResult ^. ilrContext
    , _scHoleInferState = iResult ^. ilrBaseInferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scMReinferRoot = mReinferRoot
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    }

run :: MonadA m => Context m -> SugarM m a -> T m a
run ctx (SugarM action) = runReaderT action ctx

runPure ::
  MonadA m => Anchors.CodeProps m ->
  Map Guid TagParamInfo ->
  Map Guid RecordParamsInfo ->
  SugarM m a -> T m a
runPure cp tagParamInfos recordParamsInfos act = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  run Context
    { _scMDefI = Nothing
    , _scInferState = error "pure expression doesnt have infer state"
    , _scHoleInferState = error "pure expression doesnt have hole infer state"
    , _scMContextHash = Nothing
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scMReinferRoot = Nothing
    , _scTagParamInfos = tagParamInfos
    , _scRecordParamsInfos = recordParamsInfos
    } act

readContext :: MonadA m => SugarM m (Context m)
readContext = SugarM Reader.ask

local :: Monad m => (Context m -> Context m) -> SugarM m a -> SugarM m a
local f (SugarM act) = SugarM $ Reader.local f act

liftTransaction :: MonadA m => T m a -> SugarM m a
liftTransaction = SugarM . lift

codeAnchor :: MonadA m => (Anchors.CodeProps m -> a) -> SugarM m a
codeAnchor f = f . (^. scCodeAnchors) <$> readContext

getP :: MonadA m => Transaction.MkProperty m a -> SugarM m a
getP = liftTransaction . Transaction.getP
