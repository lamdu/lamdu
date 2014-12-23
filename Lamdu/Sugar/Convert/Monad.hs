{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, PolymorphicComponents, ConstraintKinds, RecordWildCards #-}
module Lamdu.Sugar.Convert.Monad
  ( Context(..), TagParamInfo(..)
  , scInferContext, scReinferCheckDefinition, scDefI
  , scCodeAnchors, scSpecialFunctions, scTagParamInfos
  , ConvertM(..), run
  , readContext, liftTransaction, local
  , codeAnchor
  , getP
  , convertSubexpression
  , typeProtectTransaction, typeProtectedSetToVal
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Monoid (Monoid)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Val (Val)
import Lamdu.Sugar.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

data TagParamInfo = TagParamInfo
  { tpiFromParameters :: V.Var -- TODO: Rename "From" to something else
  , tpiJumpTo :: Sugar.EntityId
  }

newtype ConvertM m a = ConvertM (ReaderT (Context m) (T m) a)
  deriving (Functor, Applicative, Monad)

data Context m = Context
  { _scInferContext :: Infer.Context
  , _scDefI :: ExprIRef.DefI m
  , _scCodeAnchors :: Anchors.CodeProps m
  , _scSpecialFunctions :: Anchors.SpecialFunctions m
  , _scTagParamInfos :: Map T.Tag TagParamInfo -- tag guids
  , -- Check whether the definition is valid after an edit,
    -- so that can hole-wrap bad edits.
    _scReinferCheckDefinition :: T m Bool
  , scConvertSubexpression ::
       forall a. Monoid a => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
  }
Lens.makeLenses ''Context

typeProtectTransaction :: MonadA m => ConvertM m (T m a -> T m (Maybe a))
typeProtectTransaction =
  do
    checkOk <- (^. scReinferCheckDefinition) <$> readContext
    let
      protect act =
        do
          (resume, changes) <-
            Transaction.fork $ do
              result <- act
              isOk <- checkOk
              return $
                if isOk
                then (>> return (Just result)) . Transaction.merge
                else const $ return Nothing
          resume changes
    return protect

typeProtectedSetToVal ::
  MonadA m =>
  ConvertM m
  (ExprIRef.ValIProperty m -> ExprIRef.ValI m -> T m (ExprIRef.ValI m))
typeProtectedSetToVal = do
  typeProtect <- typeProtectTransaction
  let
    setToVal dest valI =
      do
        mResult <- typeProtect $ DataOps.replace dest valI
        case mResult of
          Just result -> return result
          Nothing -> DataOps.setToWrapper valI dest
  return setToVal

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

convertSubexpression :: (MonadA m, Monoid a) => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convertSubexpression exprI = do
  convertSub <- scConvertSubexpression <$> readContext
  convertSub exprI
