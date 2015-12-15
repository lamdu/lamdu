{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, RankNTypes, PatternGuards, RecordWildCards #-}
module Lamdu.Sugar.Convert.Binder
    ( convertBinder, convertLam
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void)
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (MkProperty)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Eval.Val (ScopeId)
import           Lamdu.Expr.IRef (ValIProperty)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), cpParams, convertParams, convertLamParams, mkStoredLam, makeDeleteLambda)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Prelude.Compat

data Redex a = Redex
    { redexBody :: Val a
    , redexBodyScope :: CurAndPrev (Map ScopeId ScopeId)
    , redexParam :: V.Var
    , redexParamRefs :: [EntityId]
    , redexArg :: Val a
    , redexHiddenPayloads :: [a]
    , redexArgAnnotation :: Annotation
    }

checkForRedex :: Val (Input.Payload m a) -> Maybe (Redex (Input.Payload m a))
checkForRedex expr = do
    V.Apply func arg <- expr ^? ExprLens.valApply
    V.Lam param body <- func ^? V.body . ExprLens._BAbs
    Just Redex
        { redexBody = body
        , redexBodyScope =
            func ^. V.payload . Input.evalResults
            <&> (^. Input.eAppliesOfLam)
            <&> Lens.traversed %~ getRedexApplies
        , redexParam = param
        , redexArg = arg
        , redexHiddenPayloads = (^. V.payload) <$> [expr, func]
        , redexArgAnnotation = makeAnnotation (arg ^. V.payload)
        , redexParamRefs = func ^. V.payload . Input.varRefsOfLambda
        }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"

mkLIActions ::
    MonadA m =>
    V.Var -> ValIProperty m -> Val (ValIProperty m) -> Val (ValIProperty m) ->
    ConvertM m (LetActions m)
mkLIActions param topLevelProp bodyStored argStored =
    do
        ext <- makeFloatLetToOuterScope param del bodyStored argStored
        return
            LetActions
            { _laSetToInner = SubExprs.getVarsToHole param bodyStored >> del
            , _laSetToHole = DataOps.setToHole topLevelProp <&> EntityId.ofValI
            , _laExtract = ext
            }
    where
        del = bodyStored ^. V.payload & replaceWith topLevelProp & void

localNewExtractDestPos ::
    MonadA m => Val (Input.Payload m x) -> ConvertM m a -> ConvertM m a
localNewExtractDestPos val =
    ConvertM.scScopeInfo . ConvertM.siOuter .~
    ConvertM.OuterScopeInfo
    { _osiPos = val ^. V.payload . Input.mStored
    , _osiVarsUnderPos = []
    }
    & ConvertM.local

localVarsUnderExtractDestPos ::
    MonadA m => [V.Var] -> ConvertM m a -> ConvertM m a
localVarsUnderExtractDestPos vars =
    ConvertM.scScopeInfo . ConvertM.siOuter . ConvertM.osiVarsUnderPos <>~ vars
    & ConvertM.local

makeInline ::
    MonadA m =>
    Maybe (ValIProperty m) -> Redex (Input.Payload m a) -> BinderVarInline m
makeInline mStored redex =
    case redexParamRefs redex of
    [_singleUsage] ->
        inlineLet (redexParam redex)
        <$> mStored
        <*> (traverse (^. Input.mStored) (redexBody redex) <&> fmap Property.value)
        <*> (traverse (^. Input.mStored) (redexArg redex) <&> fmap Property.value)
        & maybe CannotInline InlineVar
    [] -> CannotInline
    uses -> CannotInlineDueToUses uses

convertRedex ::
    (MonadA m, Monoid a) =>
    Val (Input.Payload m a) ->
    Redex (Input.Payload m a) ->
    ConvertM m (Let Guid m (ExpressionU m a))
convertRedex expr redex =
    do
        value <-
            convertBinder Nothing defGuid (redexArg redex)
            & localNewExtractDestPos expr
        actions <-
            mkLIActions param
            <$> expr ^. V.payload . Input.mStored
            <*> traverse (^. Input.mStored) (redexBody redex)
            <*> traverse (^. Input.mStored) (redexArg redex)
            & Lens.sequenceOf Lens._Just
        body <-
            makeBinderBody (redexBody redex)
            & localVarsUnderExtractDestPos [redexParam redex]
            & localNewExtractDestPos expr
            & ConvertM.local (scScopeInfo . siLetItems <>~
                Map.singleton (redexParam redex)
                (makeInline (expr ^. V.payload . Input.mStored) redex))
        Let
            { _lEntityId = defEntityId
            , _lValue =
                value
                & bBody . bbContent . SugarLens.binderContentExpr . rPayload . plData <>~
                redexHiddenPayloads redex ^. Lens.traversed . Input.userData
            , _lActions = actions
            , _lName = UniqueId.toGuid param
            , _lAnnotation = redexArgAnnotation redex
            , _lBodyScope = redexBodyScope redex
            , _lBody = body
            , _lUsages = redexParamRefs redex
            } & return
  where
      param = redexParam redex
      defGuid = UniqueId.toGuid param
      defEntityId = EntityId.ofLambdaParam param

makeBinderContent ::
    (MonadA m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderContent Guid m (ExpressionU m a))
makeBinderContent expr =
    case checkForRedex expr of
    Nothing ->
        ConvertM.convertSubexpression expr & localNewExtractDestPos expr
        <&> BinderExpr
    Just redex -> convertRedex expr redex <&> BinderLet

makeBinderBody ::
    (MonadA m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (BinderBody Guid m (ExpressionU m a))
makeBinderBody expr =
    do
        content <- makeBinderContent expr
        BinderBody
            { _bbMActions =
              expr ^. V.payload . Input.mStored
              <&> \exprProp ->
              BinderBodyActions
              { _bbaAddOuterLet =
                DataOps.redexWrap exprProp <&> EntityId.ofLambdaParam
              }
            , _bbContent = content
            } & return

makeBinder :: (MonadA m, Monoid a) =>
    Maybe (MkProperty m (Maybe BinderParamScopeId)) ->
    Maybe (MkProperty m PresentationMode) ->
    ConventionalParams m a -> Val (Input.Payload m a) ->
    ConvertM m (Binder Guid m (ExpressionU m a))
makeBinder mChosenScopeProp mPresentationModeProp ConventionalParams{..} funcBody =
    do
        binderBody <-
            makeBinderBody funcBody
            & localVarsUnderExtractDestPos (cpMLamParam ^.. Lens._Just)
        return Binder
            { _bParams = _cpParams
            , _bMPresentationModeProp = mPresentationModeProp
            , _bMChosenScopeProp = mChosenScopeProp
            , _bBody = binderBody
            , _bBodyScopes = cpScopes
            , _bMActions = cpMAddFirstParam <&> BinderActions
            }
    & ConvertM.local (ConvertM.scScopeInfo %~ addParams)
    where
        addParams ctx =
            ctx
            & ConvertM.siTagParamInfos <>~ cpParamInfos
            & ConvertM.siNullParams <>~
            case _cpParams of
            NullParam {} -> Set.fromList (cpMLamParam ^.. Lens._Just)
            _ -> Set.empty

convertLam ::
    (MonadA m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam _ lamBody) exprPl =
    do
        mDeleteLam <- mkStoredLam lam exprPl & Lens._Just %%~ makeDeleteLambda Nothing
        convParams <- convertLamParams Nothing lam exprPl
        binder <-
            makeBinder
            (exprPl ^. Input.mStored <&> Anchors.assocScopeRef . Property.value)
            Nothing convParams (lam ^. V.lamResult)
        let setToInnerExprAction =
                maybe NoInnerExpr SetToInnerExpr $
                do
                    guard $ Lens.nullOf ExprLens.valHole lamBody
                    mDeleteLam
                        <&> Lens.mapped .~ binder ^. bBody . bbContent .
                            SugarLens.binderContentExpr . rPayload . plEntityId
        let paramSet =
                binder ^.. bParams . SugarLens.binderNamedParams .
                Lens.traversed . npiName
                & Set.fromList
        let lambda
                | useNormalLambda binder =
                    Lambda NormalBinder binder
                | otherwise =
                    binder
                    & bBody . Lens.traverse %~ markLightParams paramSet
                    & Lambda LightLambda
        BodyLam lambda
            & addActions exprPl
            <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

useNormalLambda :: Binder name m (Expression name m a) -> Bool
useNormalLambda binder =
    or
    [ Lens.has (bBody . bbContent . _BinderLet) binder
    , Lens.has (bBody . Lens.traverse . SugarLens.payloadsOf forbiddenLightLamSubExprs) binder
    , Lens.nullOf (bParams . _FieldParams) binder
    ]
    where
        forbiddenLightLamSubExprs :: Lens.Fold (Body name m a) ()
        forbiddenLightLamSubExprs =
            Lens.failing (_BodyHole . check)
            (_BodyLam . lamBinder . bParams . SugarLens.binderNamedParams .
                check)
        check :: Lens.Fold a ()
        check = const () & Lens.to

markLightParams ::
    MonadA m => Set Guid -> ExpressionU m a -> ExpressionU m a
markLightParams paramNames (Expression body pl) =
    case body of
    BodyGetVar (GetParam n)
        | Set.member (n ^. pNameRef . nrName) paramNames ->
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    BodyHole h ->
        h
        & holeMActions . Lens._Just
        . holeOptions . Lens.mapped . Lens.traversed . hoResults
        . Lens.mapped . _2 . Lens.mapped . holeResultConverted
            %~ markLightParams paramNames
        & BodyHole
    _ -> body <&> markLightParams paramNames
    & (`Expression` pl)

-- Let-item or definition (form of <name> [params] = <body>)
convertBinder ::
    (MonadA m, Monoid a) =>
    Maybe V.Var -> Guid ->
    Val (Input.Payload m a) -> ConvertM m (Binder Guid m (ExpressionU m a))
convertBinder mRecursiveVar defGuid expr =
    do
        (convParams, funcBody) <- convertParams mRecursiveVar expr
        let mPresentationModeProp
                | Lens.has (cpParams . _FieldParams) convParams =
                    Just $ Anchors.assocPresentationMode defGuid
                | otherwise = Nothing
        makeBinder (Just (Anchors.assocScopeRef defGuid)) mPresentationModeProp
            convParams funcBody
