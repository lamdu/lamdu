module Lamdu.Sugar.Convert.Expression.Actions
    ( addActions, makeAnnotation
    ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

mkExtractter :: MonadA m => ExprIRef.ValI m -> T m EntityId -> T m EntityId
mkExtractter _expr replaceWithHole =
    replaceWithHole

mkReplaceWithNewHole :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
mkReplaceWithNewHole stored =
    EntityId.ofValI <$> DataOps.replaceWithHole stored

mkActions :: MonadA m => ExprIRef.ValIProperty m -> Actions m
mkActions stored =
    Actions
    { _wrap = WrapAction $ addEntityId <$> DataOps.wrap stored
    , _setToHole = SetToHole $ addEntityId <$> DataOps.setToHole stored
    , _setToInnerExpr = NoInnerExpr
    , _extract =
        Just $ -- overridden by hole conversion
        mkExtractter (Property.value stored) $ mkReplaceWithNewHole stored
    }
    where
        addEntityId valI = (UniqueId.toGuid valI, EntityId.ofValI valI)

addActions ::
    MonadA m => Input.Payload m a -> BodyU m a -> ConvertM m (ExpressionU m a)
addActions exprPl body =
    return $ Expression body Payload
        { _plEntityId = exprPl ^. Input.entityId
        , _plAnnotation = makeAnnotation exprPl
        , _plActions = mkActions  <$> exprPl ^. Input.mStored
        , _plData = exprPl ^. Input.userData
        }

makeAnnotation :: Input.Payload m a -> Annotation
makeAnnotation payload =
    Annotation
    { _aInferredType = payload ^. Input.inferred . Infer.plType
    , _aMEvaluationResult =
        do
            payload ^. Input.evalResults & Map.null & not & guard
            payload ^. Input.evalResults & Just
    }
