{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.GetVar
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Control.Monad.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, siTagParamInfos, tpiFromParameters)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

jumpToDefI ::
    Monad m => Anchors.CodeProps m -> DefI m -> Transaction m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

inlineDef ::
    Monad m => ConvertM.Context m -> V.Var -> ValIProperty m -> Transaction m EntityId
inlineDef ctx globalId dest =
    do
        def <- Transaction.readIRef defI
        case def ^. Def.defBody of
            Def.BodyBuiltin _ ->
                -- Cannot inline builtins.
                -- Jump to it instead so that user understands that it's a builtin.
                gotoDef
            Def.BodyExpr defExpr ->
                do
                    isRecursive <-
                        ExprIRef.readVal (defExpr ^. Def.expr)
                        <&> Lens.has (ExprLens.valGlobals mempty . Lens.only globalId)
                    if isRecursive
                        then gotoDef
                        else doInline def defExpr
    where
        defI = ExprIRef.defI globalId
        gotoDef = jumpToDefI (ctx ^. ConvertM.scCodeAnchors) defI
        doInline def defExpr =
            do
                Property.set dest (defExpr ^. Def.expr)
                Property.pureModify (ctx ^. ConvertM.scFrozenDeps) (<> defExpr ^. Def.exprFrozenDeps)
                newDefExpr <- DataOps.newHole
                def & Def.defBody .~ Def.BodyExpr (Def.Expr newDefExpr mempty)
                    & Transaction.writeIRef defI
                Transaction.setP (Anchors.assocDefinitionState defI) DeletedDefinition
                _ <- ctx ^. ConvertM.scPostProcessRoot
                defExpr ^. Def.expr & EntityId.ofValI & return


convertGlobal ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar UUID m)
convertGlobal param exprPl =
    do
        ctx <- lift ConvertM.readContext
        let recursiveVar =
                ctx ^? ConvertM.scScopeInfo . ConvertM.siRecursiveRef .
                Lens._Just . ConvertM.rrDefI
        let isRecursiveRef = maybe False (== defI) recursiveVar
        notInScope || isRecursiveRef & guard
        lifeState <-
            Anchors.assocDefinitionState defI
            & Transaction.getP
        let defForm =
                case lifeState of
                DeletedDefinition -> DefDeleted
                LiveDefinition ->
                    ctx ^. ConvertM.scOutdatedDefinitions . Lens.at param
                    & maybe DefUpToDate DefTypeChanged
        GetBinder BinderVar
            { _bvNameRef = NameRef
              { _nrName = UniqueId.toUUID defI
              , _nrGotoDefinition = jumpToDefI (ctx ^. ConvertM.scCodeAnchors) defI
              }
            , _bvForm = GetDefinition defForm
            , _bvInline =
                case defForm of
                DefUpToDate
                    | Lens.has (ConvertM.scInlineableDefinitions . Lens.ix param) ctx ->
                        inlineDef ctx param (exprPl ^. Input.stored)
                        & InlineVar
                _ -> CannotInline
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

convertGetLet ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar UUID m)
convertGetLet param exprPl =
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
            convertGetLet param exprPl & justToLeft
            convertParamsRecord param exprPl & justToLeft
            GetParam (Param (paramNameRef param) GetParameter NormalBinder) & return
        & runMatcherT
        <&> BodyGetVar
        >>= addActions exprPl
