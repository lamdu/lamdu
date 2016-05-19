{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, RankNTypes, RecordWildCards #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (MkProperty)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), cpParams, convertParams, convertLamParams, mkStoredLam, makeDeleteLambda)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..), checkForRedex)
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Prelude.Compat

mkLetIActions ::
    Monad m =>
    ValIProperty m -> Redex (ValIProperty m) ->
    ConvertM m (LetActions m)
mkLetIActions topLevelProp redex =
    do
        float <- makeFloatLetToOuterScope (Property.set topLevelProp) redex
        return
            LetActions
            { _laSetToInner =
                do
                    SubExprs.getVarsToHole param body
                    body ^. Val.payload & replaceWith topLevelProp & void
            , _laSetToHole = DataOps.setToHole topLevelProp <&> EntityId.ofValI
            , _laFloat = float
            }
    where
        V.Lam param body = redexLam redex

localNewExtractDestPos ::
    Monad m => Val (Input.Payload m x) -> ConvertM m a -> ConvertM m a
localNewExtractDestPos val =
    ConvertM.scScopeInfo . ConvertM.siOuter .~
    ConvertM.OuterScopeInfo
    { _osiPos = val ^. Val.payload . Input.stored & Just
    , _osiVarsUnderPos = []
    }
    & ConvertM.local

localVarsUnderExtractDestPos ::
    Monad m => [V.Var] -> ConvertM m a -> ConvertM m a
localVarsUnderExtractDestPos vars =
    ConvertM.scScopeInfo . ConvertM.siOuter . ConvertM.osiVarsUnderPos <>~ vars
    & ConvertM.local

makeInline :: Monad m => ValIProperty m -> Redex (Input.Payload m a) -> BinderVarInline m
makeInline stored redex =
    case redexParamRefs redex of
    [_singleUsage] ->
        inlineLet stored (redex <&> (^. Input.stored) <&> Property.value)
        & InlineVar
    [] -> CannotInline
    uses -> CannotInlineDueToUses uses

convertRedex ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    Redex (Input.Payload m a) ->
    ConvertM m (Let UUID m (ExpressionU m a))
convertRedex expr redex =
    do
        value <-
            convertBinder binderKind defUUID (redexArg redex)
            & localNewExtractDestPos expr
        actions <-
            mkLetIActions (expr ^. Val.payload . Input.stored)
            (redex <&> (^. Input.stored))
        letBody <-
            makeBinderBody body
            & localVarsUnderExtractDestPos [param]
            & localNewExtractDestPos expr
            & ConvertM.local (scScopeInfo . siLetItems <>~
                Map.singleton param
                (makeInline (expr ^. Val.payload . Input.stored) redex))
        ann <- redexArg redex ^. Val.payload & makeAnnotation
        return Let
            { _lEntityId = defEntityId
            , _lValue =
                value
                & bBody . bbContent . SugarLens.binderContentExpr . rPayload . plData <>~
                redexHiddenPayloads redex ^. Lens.traversed . Input.userData
            , _lActions = actions
            , _lName = UniqueId.toUUID param
            , _lAnnotation = ann
            , _lBodyScope = redexBodyScope redex
            , _lBody = letBody
            , _lUsages = redexParamRefs redex
            }
  where
      binderKind =
          redexLam redex
          <&> Lens.mapped %~ (^. Input.stored)
          & BinderKindLet
      V.Lam param body = redexLam redex
      defUUID = UniqueId.toUUID param
      defEntityId = EntityId.ofLambdaParam param

makeBinderContent ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderContent UUID m (ExpressionU m a))
makeBinderContent expr =
    case checkForRedex expr of
    Nothing ->
        ConvertM.convertSubexpression expr & localNewExtractDestPos expr
        <&> BinderExpr
    Just redex -> convertRedex expr redex <&> BinderLet

makeBinderBody ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderBody UUID m (ExpressionU m a))
makeBinderBody expr =
    do
        content <- makeBinderContent expr
        BinderBody
            { _bbAddOuterLet =
              expr ^. Val.payload . Input.stored
              & DataOps.redexWrap <&> EntityId.ofLambdaParam
            , _bbContent = content
            } & return

makeBinder :: (Monad m, Monoid a) =>
    MkProperty m (Maybe BinderParamScopeId) ->
    Maybe (MkProperty m PresentationMode) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m (Binder UUID m (ExpressionU m a))
makeBinder chosenScopeProp mPresentationModeProp ConventionalParams{..} funcBody =
    do
        binderBody <-
            makeBinderBody funcBody
            & localVarsUnderExtractDestPos (cpMLamParam ^.. Lens._Just)
        return Binder
            { _bParams = _cpParams
            , _bMPresentationModeProp = mPresentationModeProp
            , _bChosenScopeProp = chosenScopeProp
            , _bBody = binderBody
            , _bBodyScopes = cpScopes
            , _bActions = BinderActions _cpAddFirstParam
            }
    & ConvertM.local (ConvertM.scScopeInfo %~ addParams)
    where
        addParams ctx =
            ctx
            & ConvertM.siTagParamInfos <>~ _cpParamInfos
            & ConvertM.siNullParams <>~
            case _cpParams of
            NullParam {} -> Set.fromList (cpMLamParam ^.. Lens._Just)
            _ -> Set.empty

convertLam ::
    (Monad m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam _ lamBody) exprPl =
    do
        deleteLam <- mkStoredLam lam exprPl & makeDeleteLambda BinderKindLambda
        convParams <- convertLamParams lam exprPl
        binder <-
            makeBinder
            (exprPl ^. Input.stored & Property.value & Anchors.assocScopeRef)
            Nothing convParams (lam ^. V.lamResult)
        let setToInnerExprAction
                | Lens.nullOf ExprLens.valHole lamBody =
                  binder ^. bBody . bbContent . SugarLens.binderContentExpr .
                  rPayload . plEntityId
                  <$ deleteLam
                  & SetToInnerExpr
                | otherwise = NoInnerExpr
        let paramUUIDs =
                binder ^.. bParams . SugarLens.binderNamedParams .
                Lens.traversed . npiName
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
            <&> rPayload . plActions . setToInnerExpr .~ setToInnerExprAction

useNormalLambda :: Set UUID -> Binder UUID m (Expression UUID m a) -> Bool
useNormalLambda paramUUIDs binder =
    any (binder &)
    [ Lens.hasn't (bParams . _FieldParams)
    , Lens.has (bBody . bbContent . _BinderLet)
    , Lens.has (bBody . Lens.traverse . SugarLens.payloadsOf forbiddenLightLamSubExprs)
    , not . allParamsUsed paramUUIDs
    ]
    where
        forbiddenLightLamSubExprs :: Lens.Fold (Body name m a) ()
        forbiddenLightLamSubExprs =
            Lens.failing (_BodyHole . check)
            (_BodyLam . lamBinder . bParams . SugarLens.binderNamedParams .
                check)
        check :: Lens.Fold a ()
        check = const () & Lens.to

allParamsUsed :: Set UUID -> Binder UUID m (Expression UUID m a) -> Bool
allParamsUsed paramUUIDs binder =
    Set.null (paramUUIDs `Set.difference` usedParams)
    where
        usedParams =
            binder ^.. Lens.traverse . SugarLens.subExprPayloads . Lens.asIndex .
            rBody . _BodyGetVar . _GetParam . pNameRef . nrName
            & Set.fromList

markLightParams ::
    Monad m => Set UUID -> ExpressionU m a -> ExpressionU m a
markLightParams paramUUIDs (Expression body pl) =
    case body of
    BodyGetVar (GetParam n)
        | Set.member (n ^. pNameRef . nrName) paramUUIDs ->
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    BodyHole h ->
        h
        & holeActions . holeOptions . Lens.mapped . Lens.traversed . hoResults
        . Lens.mapped . _2 . Lens.mapped . holeResultConverted
            %~ markLightParams paramUUIDs
        & BodyHole
    _ -> body <&> markLightParams paramUUIDs
    & (`Expression` pl)

-- Let-item or definition (form of <name> [params] = <body>)
convertBinder ::
    (Monad m, Monoid a) => BinderKind m -> UUID ->
    Val (Input.Payload m a) -> ConvertM m (Binder UUID m (ExpressionU m a))
convertBinder binderKind defUUID expr =
    do
        (convParams, funcBody) <- convertParams binderKind expr
        let mPresentationModeProp
                | Lens.has (cpParams . _FieldParams) convParams =
                    Just $ Anchors.assocPresentationMode defUUID
                | otherwise = Nothing
        makeBinder (Anchors.assocScopeRef defUUID) mPresentationModeProp
            convParams funcBody

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Val (Input.Payload m a) ->
    ConvertM m (Binder UUID m (ExpressionU m a))
convertDefinitionBinder defI =
    convertBinder (BinderKindDef defI) (IRef.uuid defI)
