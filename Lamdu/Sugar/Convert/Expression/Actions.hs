{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Expression.Actions
    ( addActions, makeAnnotation
    , makeSetToInner
    , addActionsWithSetToInner
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

mkExtractor :: MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> T m EntityId
mkExtractor extractDestPos stored =
    do
        (lamI, getVarI) <-
            if Property.value stored == Property.value extractDestPos
            then
                -- Give entire binder body a name (replace binder body
                -- with "(\x -> x) stored")
                DataOps.newIdentityLambda
            else
                -- Give some subexpr in binder body a name (replace
                -- binder body with "(\x -> binderBody) stored", and
                -- stored becomes "x")
                do
                    (newParam, lamI) <-
                        Property.value extractDestPos & DataOps.newLambda
                    getVarI <- V.LVar newParam & V.BLeaf & ExprIRef.newValBody
                    Property.set stored getVarI
                    return (lamI, getVarI)
        V.Apply lamI oldStored & V.BApp & ExprIRef.newValBody
            >>= Property.set extractDestPos
        EntityId.ofValI getVarI & return
    where
        oldStored = Property.value stored

mkActions ::
    MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> Actions m
mkActions extractDestPos stored =
    Actions
    { _wrap = DataOps.wrap stored <&> addEntityId & WrapAction
    , _setToHole = DataOps.setToHole stored <&> addEntityId & SetToHole
    , _setToInnerExpr = NoInnerExpr
    , _extract =
        Just $ -- overridden by hole conversion
        mkExtractor extractDestPos stored
    }
    where
        addEntityId valI = (UniqueId.toGuid valI, EntityId.ofValI valI)

makeSetToInnerStored ::
    Monad m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m ->
    ConvertM m (Transaction m EntityId)
makeSetToInnerStored outerProp innerProp =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        protectedSetToVal outerProp (Property.value innerProp)
            <&> EntityId.ofValI
            & return

makeSetToInner ::
    Monad m => Input.Payload m a -> V.Val (Input.Payload m b) ->
    ConvertM m (SetToInnerExpr m)
makeSetToInner outerPl inner
    | Lens.nullOf ExprLens.valHole inner =
      makeSetToInnerStored
      <$> outerPl ^. Input.mStored
      <*> inner ^. V.payload . Input.mStored
      & maybe (return NoInnerExpr) (fmap SetToInnerExpr)
    | otherwise = return NoInnerExpr

addActions ::
    MonadA m => Input.Payload m a -> BodyU m a -> ConvertM m (ExpressionU m a)
addActions exprPl body =
    do
        mExtractDestPos <- ConvertM.readContext <&> (^. ConvertM.scMExtractDestPos)
        return $ Expression body Payload
            { _plEntityId = exprPl ^. Input.entityId
            , _plAnnotation = makeAnnotation exprPl
            , _plActions = mkActions <$> mExtractDestPos <*> exprPl ^. Input.mStored
            , _plData = exprPl ^. Input.userData
            }

addActionsWithSetToInner ::
    MonadA m =>
    Input.Payload m a -> V.Val (Input.Payload m b) ->
    BodyU m a -> ConvertM m (ExpressionU m a)
addActionsWithSetToInner exprPl inner body =
    do
        setToInner <- makeSetToInner exprPl inner
        addActions exprPl body
            <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInner

makeAnnotation :: Input.Payload m a -> Annotation
makeAnnotation payload =
    Annotation
    { _aInferredType = payload ^. Input.inferredType
    , _aMEvaluationResult =
        payload ^. Input.evalResults <&> (^. Input.eResults) <&> mk
    }
    where
        mk res =
            do
                Map.null res & not & guard
                Just res
