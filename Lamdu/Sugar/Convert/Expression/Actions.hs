module Lamdu.Sugar.Convert.Expression.Actions
  ( addActions
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

mkCutter ::
  MonadA m => Anchors.CodeProps m -> ExprIRef.ValI m -> T m EntityId -> T m EntityId
mkCutter cp expr replaceWithHole = do
  _ <- DataOps.newClipboard cp expr
  replaceWithHole

mkReplaceWithNewHole :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
mkReplaceWithNewHole stored =
  EntityId.ofValI <$> DataOps.replaceWithHole stored

mkActions :: MonadA m => ConvertM.Context m -> ExprIRef.ValIProperty m -> Actions m
mkActions sugarContext stored =
  Actions
  { _wrap = WrapAction $ addEntityId <$> DataOps.wrap stored
  , _setToHole = SetToHole $ addEntityId <$> DataOps.setToHole stored
  , _setToInnerExpr = NoInnerExpr
  , _cut =
    Just $ -- overridden by hole conversion
    mkCutter (sugarContext ^. ConvertM.scCodeAnchors)
    (Property.value stored) $ mkReplaceWithNewHole stored
  }
  where
    addEntityId valI = (UniqueId.toGuid valI, EntityId.ofValI valI)

addActions ::
  MonadA m => Input.Payload m a -> BodyU m a -> ConvertM m (ExpressionU m a)
addActions exprPl body = do
  sugarContext <- ConvertM.readContext
  return $ Expression body Payload
    { _plEntityId = exprPl ^. Input.entityId
    , _plInferredType = exprPl ^. Input.inferred . Infer.plType
    , _plActions = mkActions sugarContext <$> exprPl ^. Input.mStored
    , _plData = exprPl ^. Input.userData
    }
