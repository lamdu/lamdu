{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.GetVar
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.UUID.Types (UUID)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import           Lamdu.Sugar.Convert.Monad (ConvertM, siTagParamInfos, tpiFromParameters)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

jumpToDefI ::
    Monad m => Anchors.CodeProps m -> DefI m -> Transaction m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

convertGlobal ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar UUID m)
convertGlobal param exprPl =
    do
        ctx <- lift ConvertM.readContext
        let isGlobalInScope =
                ctx ^. ConvertM.scGlobalsInScope . Lens.contains defI
        notInScope || isGlobalInScope & guard
        defState <-
            Anchors.assocDefinitionState defI
            & Transaction.getP & ConvertM.liftTransaction & lift
        GetBinder BinderVar
            { _bvNameRef = NameRef
              { _nrName = UniqueId.toUUID defI
              , _nrGotoDefinition =
                  jumpToDefI (ctx ^. ConvertM.scCodeAnchors) defI
              }
            , _bvForm = DefinitionForm defState & GetDefinition
            , _bvInline = CannotInline
            } & return
    where
        defI = ExprIRef.defI param
        notInScope =
            exprPl ^. Input.inferred . Infer.plScope
            & Infer.scopeToTypeMap
            & Lens.has (Lens.at param . Lens._Nothing)

usesAround :: Eq a => a -> [a] -> [a]
usesAround x xs =
    drop 1 after ++ before
    where
        (before, after) = break (== x) xs

paramNameRef :: Monad m => V.Var -> NameRef UUID m
paramNameRef param =
    NameRef
    { _nrName = UniqueId.toUUID param
    , _nrGotoDefinition = pure $ EntityId.ofLambdaParam param
    }

convertGetBinder ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar UUID m)
convertGetBinder param exprPl =
    do
        inline <-
            lift ConvertM.readContext
            <&> (^. ConvertM.scScopeInfo . ConvertM.siLetItems . Lens.at param)
            >>= maybeToMPlus
        GetBinder BinderVar
            { _bvNameRef = paramNameRef param
            , _bvForm = GetLet
            , _bvInline =
                inline
                & _CannotInlineDueToUses %~ usesAround (exprPl ^. Input.entityId)
            } & return

convertParamsRecord ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar UUID m)
convertParamsRecord param exprPl =
    do
        lift ConvertM.readContext
            <&> (^.. ConvertM.scScopeInfo . siTagParamInfos . Lens.traversed
                . ConvertM._TagFieldParam)
            <&> map tpiFromParameters
            <&> elem param
            >>= guard
        GetParamsRecord ParamsRecordVar
            { _prvFieldNames =
                exprPl
                ^.. Input.inferredType . T._TRecord . ExprLens.compositeFieldTags
                <&> UniqueId.toUUID
            } & return

convert ::
    Monad m =>
    V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert param exprPl
    | param == ConvertHole.injectVar =
        addActions exprPl BodyInjectedExpression
    | otherwise =
        do
            convertGlobal param exprPl & justToLeft
            convertGetBinder param exprPl & justToLeft
            convertParamsRecord param exprPl & justToLeft
            GetParam (Param (paramNameRef param) GetParameter NormalBinder) & return
        & runMatcherT
        <&> BodyGetVar
        >>= addActions exprPl
