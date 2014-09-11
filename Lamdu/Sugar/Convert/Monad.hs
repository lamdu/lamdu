{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, PolymorphicComponents, ConstraintKinds, RecordWildCards #-}
module Lamdu.Sugar.Convert.Monad
  ( Context(..), TagParamInfo(..), RecordParamsInfo(..)
  , scInferContext
  , scCodeAnchors, scSpecialFunctions, scTagParamInfos, scRecordParamsInfos
  , ConvertM(..), run
  , readContext, liftTransaction, local
  , codeAnchor
  , getP
  , convertSubexpression
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Types as Sugar

data TagParamInfo = TagParamInfo
  { tpiFromParameters :: V.Var -- TODO: Rename "From" to something else
  , tpiJumpTo :: Guid
  }

data RecordParamsInfo m = RecordParamsInfo
  { rpiFromDefinition :: Guid
  , rpiJumpTo :: T m Guid
  }

newtype ConvertM m a = ConvertM (ReaderT (Context m) (T m) a)
  deriving (Functor, Applicative, Monad)

data Context m = Context
  { _scInferContext :: Infer.Context
  , _scCodeAnchors :: Anchors.CodeProps m
  , _scSpecialFunctions :: Anchors.SpecialFunctions (Tag m)
  , _scTagParamInfos :: Map T.Tag TagParamInfo -- tag guids
  , _scRecordParamsInfos :: Map V.Var (RecordParamsInfo m) -- param guids
  , scConvertSubexpression ::
       forall a. Monoid a => Sugar.InputExpr m a -> ConvertM m (ExpressionU m a)
  }
Lens.makeLenses ''Context


run :: MonadA m => Context m -> ConvertM m a -> T m a
run ctx (ConvertM action) = runReaderT action ctx

readContext :: MonadA m => ConvertM m (Context m)
readContext = ConvertM Reader.ask

local :: Monad m => (Context m -> Context m) -> ConvertM m a -> ConvertM m a
local f (ConvertM act) = ConvertM $ Reader.local f act

liftTransaction :: MonadA m => T m a -> ConvertM m a
liftTransaction = ConvertM . lift

codeAnchor :: MonadA m => (Anchors.CodeProps m -> a) -> ConvertM m a
codeAnchor f = f . (^. scCodeAnchors) <$> readContext

getP :: MonadA m => Transaction.MkProperty m a -> ConvertM m a
getP = liftTransaction . Transaction.getP

convertSubexpression :: (MonadA m, Monoid a) => Sugar.InputExpr m a -> ConvertM m (ExpressionU m a)
convertSubexpression exprI = do
  convertSub <- scConvertSubexpression <$> readContext
  convertSub exprI
