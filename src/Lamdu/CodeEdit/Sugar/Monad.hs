{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.Monad
  ( Context(..), TagParamInfo(..), RecordParamsInfo(..)
  , scMDefI, scInferState, scContextHash, scHoleInferState
  , scCodeAnchors, scSpecialFunctions, scMReinferRoot, scTagParamInfos, scRecordParamsInfos, scConvertSubexpression
  , SugarM(..), run
  , readContext, liftTransaction, local
  , codeAnchor
  , getP
  , convertSubexpression
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Lamdu.CodeEdit.Sugar.Infer (ExprMM)
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

data RecordParamsInfo m = RecordParamsInfo
  { rpiFromDefinition :: Guid
  , rpiJumpTo :: T m Guid
  }

data Context m = Context
  { _scMDefI :: Maybe (DefI (Tag m))
  , _scInferState :: Infer.Context (DefI (Tag m))
  , _scContextHash :: Cache.KeyBS
  , _scHoleInferState :: Infer.Context (DefI (Tag m))
  , _scCodeAnchors :: Anchors.CodeProps m
  , _scSpecialFunctions :: Anchors.SpecialFunctions (Tag m)
  , _scMReinferRoot :: Maybe (String -> CT m Bool)
  , _scTagParamInfos :: Map Guid TagParamInfo -- tag guids
  , _scRecordParamsInfos :: Map Guid (RecordParamsInfo m) -- param guids
  , _scConvertSubexpression :: ExprMM m -> SugarM m (ExpressionU m)
  }

newtype SugarM m a = SugarM (ReaderT (Context m) (T m) a)
  deriving (Functor, Applicative, Monad)

LensTH.makeLenses ''Context

run :: MonadA m => Context m -> SugarM m a -> T m a
run ctx (SugarM action) = runReaderT action ctx

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

convertSubexpression :: MonadA m => ExprMM m -> SugarM m (ExpressionU m)
convertSubexpression exprI = do
  convertSub <- (^. scConvertSubexpression) <$> readContext
  convertSub exprI
