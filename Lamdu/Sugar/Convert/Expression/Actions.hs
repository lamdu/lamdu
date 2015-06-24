module Lamdu.Sugar.Convert.Expression.Actions
    ( addActions, makeAnnotation
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

mkExtractter :: MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> T m EntityId
mkExtractter bodyStored stored =
    do
        (lamI, getVarI) <-
            if Property.value stored == Property.value bodyStored
            then
                do
                    -- Create temporary hole to give to newLambda
                    -- because we want to know the param to set its value.
                    newBodyI <- DataOps.newHole
                    (newParam, lamI) <- ExprIRef.newLambda newBodyI
                    V.LVar newParam & V.BLeaf & ExprIRef.writeValBody newBodyI
                    return (lamI, newBodyI)
            else
                do
                    (newParam, lamI) <-
                        ExprIRef.newLambda (Property.value bodyStored)
                    getVarI <- V.LVar newParam & V.BLeaf & ExprIRef.newValBody
                    Property.set stored getVarI
                    return (lamI, getVarI)
        Property.value stored & V.Apply lamI & V.BApp & ExprIRef.newValBody
            >>= Property.set bodyStored
        EntityId.ofValI getVarI & return

mkActions ::
    MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> Actions m
mkActions bodyStored stored =
    Actions
    { _wrap = WrapAction $ addEntityId <$> DataOps.wrap stored
    , _setToHole = SetToHole $ addEntityId <$> DataOps.setToHole stored
    , _setToInnerExpr = NoInnerExpr
    , _extract =
        Just $ -- overridden by hole conversion
        mkExtractter bodyStored stored
    }
    where
        addEntityId valI = (UniqueId.toGuid valI, EntityId.ofValI valI)

addActions ::
    MonadA m => Input.Payload m a -> BodyU m a -> ConvertM m (ExpressionU m a)
addActions exprPl body =
    do
        mBodyStored <- ConvertM.readContext <&> (^. ConvertM.scMBodyStored)
        return $ Expression body Payload
            { _plEntityId = exprPl ^. Input.entityId
            , _plAnnotation = makeAnnotation exprPl
            , _plActions = mkActions <$> mBodyStored <*> exprPl ^. Input.mStored
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
