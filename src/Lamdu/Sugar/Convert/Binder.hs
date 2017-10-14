{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, TypeFamilies, RankNTypes, DisambiguateRecordFields #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam, convertBinderBody
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty, mkProperty)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertParams, convertLamParams)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    V.Lam (Val (Input.Payload m a)) -> T m ()
lamParamToHole (V.Lam param body) =
    SubExprs.getVarsToHole param (body <&> (^. Input.stored))

mkLetItemActions ::
    Monad m =>
    ValIProperty m -> Redex (Input.Payload m a) ->
    ConvertM m (LetActions (T m))
mkLetItemActions topLevelProp redex =
    do
        float <- makeFloatLetToOuterScope (Property.set topLevelProp) redex
        postProcess <- ConvertM.postProcess
        return
            LetActions
            { _laSetToInner =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored
                        & replaceWith topLevelProp & void
                <* postProcess
            , _laSetToHole =
                DataOps.setToHole topLevelProp
                <* postProcess
                <&> EntityId.ofValI
            , _laFloat = float
            , _laWrap = DataOps.wrap topLevelProp <* postProcess <&> addEntityId
            }
    where
        addEntityId valI = (UniqueId.toUUID valI, EntityId.ofValI valI)

localNewExtractDestPos ::
    Val (Input.Payload m x) -> ConvertM m a -> ConvertM m a
localNewExtractDestPos val =
    ConvertM.scScopeInfo . ConvertM.siMOuter .~
    Just ConvertM.OuterScopeInfo
    { _osiPos = val ^. Val.payload . Input.stored
    , _osiScope = val ^. Val.payload . Input.inferred . Infer.plScope
    }
    & ConvertM.local

makeInline :: Monad m => ValIProperty m -> Redex (Input.Payload m a) -> BinderVarInline (T m)
makeInline stored redex =
    case redex ^. Redex.paramRefs of
    [_singleUsage] ->
        inlineLet stored (redex <&> (^. Input.stored) <&> Property.value)
        & InlineVar
    [] -> CannotInline
    uses -> CannotInlineDueToUses uses

convertRedex ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    Redex (Input.Payload m a) ->
    ConvertM m (Let UUID (T m) (ExpressionU m a))
convertRedex expr redex =
    do
        (_pMode, value) <-
            convertBinder binderKind defUUID (redex ^. Redex.arg)
            & localNewExtractDestPos expr
        actions <- mkLetItemActions (expr ^. Val.payload . Input.stored) redex
        letBody <-
            convertBinderBody body
            & localNewExtractDestPos expr
            & ConvertM.local (scScopeInfo . siLetItems <>~
                Map.singleton param
                (makeInline (expr ^. Val.payload . Input.stored) redex))
        ann <- redex ^. Redex.arg . Val.payload & makeAnnotation
        return Let
            { _lEntityId = defEntityId
            , _lValue = value
            , _lActions = actions
            , _lName = UniqueId.toUUID param
            , _lAnnotation = ann
            , _lBodyScope = redex ^. Redex.bodyScope
            , _lBody = letBody
            , _lUsages = redex ^. Redex.paramRefs
            }
  where
      binderKind =
          redex ^. Redex.lam
          <&> Lens.mapped %~ (^. Input.stored)
          & BinderKindLet
      V.Lam param body = redex ^. Redex.lam
      defUUID = UniqueId.toUUID param
      defEntityId = EntityId.ofLambdaParam param

makeBinderContent ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderContent UUID (T m) (ExpressionU m a))
makeBinderContent expr =
    case Redex.check expr of
    Nothing ->
        ConvertM.convertSubexpression expr & localNewExtractDestPos expr
        <&> BinderExpr
    Just redex -> convertRedex expr redex <&> BinderLet

convertBinderBody ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderBody UUID (T m) (ExpressionU m a))
convertBinderBody expr =
    do
        content <- makeBinderContent expr
        BinderBody
            { _bbAddOuterLet =
              expr ^. Val.payload . Input.stored
              & DataOps.redexWrap <&> EntityId.ofLambdaParam
            , _bbContent = content
            } & return

makeBinder ::
    (Monad m, Monoid a) =>
    MkProperty m (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m (Binder UUID (T m) (ExpressionU m a))
makeBinder chosenScopeProp params funcBody =
    do
        binderBody <- convertBinderBody funcBody
        return Binder
            { _bParams = _cpParams params
            , _bChosenScopeProp = chosenScopeProp ^. mkProperty
            , _bLamId = cpMLamParam params ^? Lens._Just . _1
            , _bBody = binderBody
            , _bBodyScopes = cpScopes params
            , _bActions = BinderActions (_cpAddFirstParam params)
            }
    & ConvertM.local (ConvertM.scScopeInfo %~ addParams)
    where
        addParams ctx =
            ctx
            & ConvertM.siTagParamInfos <>~ _cpParamInfos params
            & ConvertM.siNullParams <>~
            case _cpParams params of
            NullParam {} -> Set.fromList (cpMLamParam params ^.. Lens._Just . _2)
            _ -> Set.empty

convertLam ::
    (Monad m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam exprPl =
    do
        convParams <- convertLamParams lam exprPl
        binder <-
            makeBinder
            (exprPl ^. Input.stored & Property.value & Anchors.assocScopeRef)
            convParams (lam ^. V.lamResult)
        let paramUUIDs =
                binder ^.. bParams . _FieldParams . traverse . fpInfo . fpiTag . tagName
                & Set.fromList
        let lambda
                | useNormalLambda paramUUIDs binder =
                    Lambda NormalBinder binder
                | otherwise =
                    binder
                    & bBody . Lens.traverse %~ markLightParams paramUUIDs
                    & Lambda LightLambda
        BodyLam lambda
            & addActions exprPl
            <&> rBody . Lens.mapped . rPayload . plActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)

useNormalLambda :: Set UUID -> Binder UUID (T m) (Expression UUID (T m) a) -> Bool
useNormalLambda paramUUIDs binder =
    any (binder &)
    [ Lens.hasn't (bParams . _FieldParams)
    , Lens.has (bBody . bbContent . _BinderLet)
    , Lens.has (bBody . Lens.traverse . SugarLens.payloadsOf forbiddenLightLamSubExprs)
    , not . allParamsUsed paramUUIDs
    ]
    where
        forbiddenLightLamSubExprs :: Lens.Traversal' (Body name m a) ()
        forbiddenLightLamSubExprs =
            Lens.failing (_BodyHole . Lens.united)
            (_BodyLam . lamBinder . bParams . namedParams .
             Lens.united)
        namedParams :: Lens.Traversal' (BinderParams name m) ()
        namedParams = Lens.failing (_VarParam . Lens.united) (_FieldParams . Lens.united)

allParamsUsed :: Set UUID -> Binder UUID (T m) (Expression UUID (T m) a) -> Bool
allParamsUsed paramUUIDs binder =
    Set.null (paramUUIDs `Set.difference` usedParams)
    where
        usedParams =
            binder ^.. Lens.traverse . SugarLens.subExprPayloads . Lens.asIndex .
            rBody . _BodyGetVar . _GetParam . pNameRef . nrName
            & Set.fromList

markLightParams ::
    Monad m => Set UUID -> Expression UUID (T m) a -> Expression UUID (T m) a
markLightParams paramUUIDs (Expression body pl) =
    case body of
    BodyGetVar (GetParam n)
        | Set.member (n ^. pNameRef . nrName) paramUUIDs ->
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    BodyHole h ->
        h
        & holeActions . Lens.mapped %~ markLightParams paramUUIDs
        & BodyHole
    _ -> body <&> markLightParams paramUUIDs
    & (`Expression` pl)

-- Let-item or definition (form of <name> [params] = <body>)
convertBinder ::
    (Monad m, Monoid a) =>
    BinderKind m -> UUID -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty m PresentationMode)
    , Binder UUID (T m) (ExpressionU m a)
    )
convertBinder binderKind defUUID expr =
    do
        (mPresentationModeProp, convParams, funcBody) <- convertParams binderKind defUUID expr
        makeBinder (Anchors.assocScopeRef defUUID) convParams funcBody
            <&> (,) mPresentationModeProp

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty m PresentationMode)
    , Binder UUID (T m) (ExpressionU m a)
    )
convertDefinitionBinder defI =
    convertBinder (BinderKindDef defI) (IRef.uuid defI)
