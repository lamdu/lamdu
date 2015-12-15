{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

toGetGlobal :: MonadA m => DefI m -> ValIProperty m -> Transaction m ()
toGetGlobal defI exprP =
    ExprIRef.writeValBody exprI $ V.BLeaf $ V.LGlobal globalId
    where
        exprI = Property.value exprP
        globalId = ExprIRef.globalId defI

moveToGlobalScope ::
    MonadA m =>
    ConvertM.Context m -> V.Var -> Val (ValIProperty m) ->
    Transaction m (DefI m)
moveToGlobalScope ctx param argStored =
    do
        paramName <- Anchors.assocNameRef param & Transaction.getP
        SubExprs.onGetVars
            (toGetGlobal
             (fromMaybe (error "recurseVar used not in definition context?!") (ctx ^. ConvertM.scDefI)))
            Builtins.recurseVar argStored
        DataOps.newPublicDefinitionWithPane paramName
            (ctx ^. ConvertM.scCodeAnchors)
            (Property.value (argStored ^. V.payload))

floatLetToOuterScope ::
    MonadA m =>
    ConvertM.Context m ->
    V.Var -> ValIProperty m -> Val (ValIProperty m) -> Val (ValIProperty m) ->
    Transaction m EntityId
floatLetToOuterScope ctx param topLevelProp bodyStored argStored =
    do
        outerScopeInfo ^. ConvertM.osiVarsUnderPos
            & mapM_ (`SubExprs.getVarsToHole` argStored)
        (newLeafBody, resultEntity) <-
            case outerScopeInfo ^. ConvertM.osiPos of
            Nothing ->
                do
                    newDefI <- moveToGlobalScope ctx param argStored
                    return
                        ( ExprIRef.globalId newDefI & V.LGlobal
                        , EntityId.ofIRef newDefI
                        )
            Just outerScope ->
                (V.LVar param, EntityId.ofLambdaParam param) <$
                DataOps.redexWrapWithGivenParam param
                (Property.value (argStored ^. V.payload)) outerScope
        let newBody = V.BLeaf newLeafBody
        _ <-
            ExprIRef.writeValWithStoredSubexpressions
            (Property.value topLevelProp)
            (go newBody (bodyStored <&> Just . Property.value) <&> flip (,) ())
        return resultEntity
    where
        outerScopeInfo = ctx ^. ConvertM.scScopeInfo . ConvertM.siOuter
        go newBody (Val s (V.BLeaf (V.LVar v))) | v == param =
            Val s newBody
        go newBody val = val & V.body . Lens.mapped %~ go newBody

makeFloatLetToOuterScope ::
    MonadA m =>
    V.Var -> ValIProperty m -> Val (ValIProperty m) -> Val (ValIProperty m) ->
    ConvertM m (Transaction m EntityId)
makeFloatLetToOuterScope param topLevelProp bodyStored argStored =
    do
        ctx <- ConvertM.readContext
        floatLetToOuterScope ctx param topLevelProp bodyStored argStored
            & return
