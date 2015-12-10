{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Extract
    ( extractLetToOuterScope
    ) where

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
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

extractLetToOuterScope ::
    MonadA m =>
    Maybe (ValIProperty m) ->
    Maybe (DefI m) ->
    Anchors.CodeProps m ->
    [V.Var] -> V.Var -> Transaction m () ->
    Val (ValIProperty m) -> Val (ValIProperty m) ->
    Transaction m EntityId
extractLetToOuterScope mExtractDestPos mRecursiveDefI cp binderScopeVars param delItem bodyStored argStored =
    do
        mapM_ (`SubExprs.getVarsToHole` argStored) binderScopeVars
        delItem
        case mExtractDestPos of
            Nothing ->
                do
                    paramName <- Anchors.assocNameRef param & Transaction.getP
                    SubExprs.onGetVars
                        (SubExprs.toGetGlobal
                         (fromMaybe (error "recurseVar used not in definition context?!") mRecursiveDefI))
                        Builtins.recurseVar argStored
                    newDefI <- DataOps.newPublicDefinitionWithPane paramName cp extractedI
                    SubExprs.onGetVars (SubExprs.toGetGlobal newDefI) param bodyStored
                    EntityId.ofIRef newDefI & return
            Just scopeBodyP ->
                EntityId.ofLambdaParam param <$
                DataOps.redexWrapWithGivenParam param extractedI scopeBodyP
    where
        extractedI = argStored ^. V.payload & Property.value
