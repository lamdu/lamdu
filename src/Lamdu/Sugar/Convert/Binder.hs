{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertParams, convertLamParams, cpParams, cpAddFirstParam, mkVarInfo)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTaggedEntity)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    V.Lam (Val (Input.Payload m a)) -> T m ()
lamParamToHole (V.Lam param x) =
    SubExprs.getVarsToHole param (x <&> (^. Input.stored))

mkLetItemActions ::
    Monad m =>
    Input.Payload m a -> Redex (Input.Payload m a) ->
    ConvertM m (LetActions InternalName (T m) (T m))
mkLetItemActions topLevelPl redex =
    do
        postProcess <- ConvertM.postProcessAssert
        nodeActions <- makeActions topLevelPl
        pure LetActions
            { _laDelete =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored
                        & replaceWith topLevelProp & void
                <* postProcess
            , _laNodeActions = nodeActions
            }
    where
        topLevelProp = topLevelPl ^. Input.stored

localNewExtractDestPos ::
    Val (Input.Payload m x) -> ConvertM m a -> ConvertM m a
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. Val.payload . Input.stored
    , _osiScope = x ^. Val.payload . Input.inferred . Infer.plScope
    }
    & ConvertM.local

makeInline ::
    Monad m =>
    ValP m -> Redex (Input.Payload m a) -> EntityId -> BinderVarInline (T m)
makeInline stored redex useId
    | Lens.has traverse otherUses = CannotInlineDueToUses (drop 1 after ++ before)
    | otherwise =
        inlineLet stored (redex <&> (^. Input.stored) <&> Property.value)
        & InlineVar
    where
        otherUses = filter (/= useId) uses
        uses = redex ^. Redex.paramRefs
        (before, after) = break (== useId) uses

-- Differs from SugarLens version by Payload type
-- TODO: cleanup
binderEntityId ::
    Lens' (Binder name i o (ConvertPayload m a)) EntityId
binderEntityId f (BinderExpr e) =
    e & _PNode . ann . pInput . Input.entityId %%~ f <&> BinderExpr
binderEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

convertRedex ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    Redex (Input.Payload m a) ->
    ConvertM m (Let InternalName (T m) (T m) (ConvertPayload m a))
convertRedex expr redex =
    do
        tag <- convertTaggedEntity param
        (_pMode, value) <-
            convertAssignment binderKind param (redex ^. Redex.arg)
            & localNewExtractDestPos expr
        actions <-
            mkLetItemActions (expr ^. Val.payload) redex
            & localNewExtractDestPos expr
        letBody <-
            convertBinder bod
            & localNewExtractDestPos expr
            & ConvertM.local (scScopeInfo . siLetItems <>~
                Map.singleton param (makeInline stored redex))
        float <- makeFloatLetToOuterScope (stored ^. Property.pSet) redex
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . Val.payload . Input.stored . Property.pVal)
                        <&> EntityId.ofValI
                    )
        pure Let
            { _lEntityId = EntityId.ofBinder param
            , _lVarInfo = redex ^. Redex.arg . Val.payload . Input.inferred . Infer.plType & mkVarInfo
            , _lValue = value & aNodeActions %~ fixValueNodeActions
            , _lActions = actions
            , _lName = tag
            , _lBodyScope = redex ^. Redex.bodyScope
            , _lBody =
                letBody
                & Lens.failing
                    (_BinderExpr . _PNode . ann . pActions)
                    (_BinderLet . lActions . laNodeActions) . mReplaceParent ?~
                    (letBody ^. binderEntityId <$ actions ^. laDelete)
            , _lUsages = redex ^. Redex.paramRefs
            }
    where
        stored = expr ^. Val.payload . Input.stored
        binderKind =
            redex ^. Redex.lam
            <&> Lens.mapped %~ (^. Input.stored)
            & BinderKindLet
        V.Lam param bod = redex ^. Redex.lam

convertBinder ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (Binder InternalName (T m) (T m) (ConvertPayload m a))
convertBinder expr =
    case Redex.check expr of
    Nothing ->
        ConvertM.convertSubexpression expr & localNewExtractDestPos expr
        <&> BinderExpr
    Just redex -> convertRedex expr redex <&> BinderLet

makeFunction ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m (Function InternalName (T m) (T m) (ConvertPayload m a))
makeFunction chosenScopeProp params funcBody =
    convertBinder funcBody
    <&> mkRes
    & ConvertM.local (ConvertM.scScopeInfo %~ addParams)
    where
        mkRes assignmentBody =
            Function
            { _fParams =
                -- TODO: avoid partiality here
                params ^?! cpParams . Lens._Just
            , _fChosenScopeProp = chosenScopeProp ^. Property.mkProperty
            , _fBody = assignmentBody
            , _fBodyScopes = cpScopes params
            , _fAddFirstParam = params ^. cpAddFirstParam
            }
        addParams ctx =
            ctx
            & ConvertM.siTagParamInfos <>~ _cpParamInfos params
            & ConvertM.siNullParams <>~
            case params ^. cpParams of
            Just NullParam{} -> Set.fromList (cpMLamParam params ^.. Lens._Just . _2)
            _ -> Set.empty

makeAssignment ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) -> Input.Payload m a ->
    ConvertM m (Assignment InternalName (T m) (T m) (ConvertPayload m a))
makeAssignment chosenScopeProp params funcBody pl =
    do
        bod <-
            case params ^. cpParams of
            Nothing ->
                convertBinder funcBody
                <&> AssignPlain (params ^. cpAddFirstParam)
                <&> BodyPlain
            Just{} ->
                makeFunction chosenScopeProp params funcBody
                <&> AssignFunction (cpMLamParam params ^?! Lens._Just . _1)
                <&> BodyFunction
        nodeActions <- makeActions pl
        let mRemoveSetToHole
                | Lens.has (_BodyPlain . apBody . _BinderExpr . _PNode . val . _BodyHole) bod =
                    mSetToHole .~ Nothing
                | otherwise = id
        pure Assignment
            { _aNodeActions = mRemoveSetToHole nodeActions
            , _aBody = bod
            }

convertLam ::
    (Monad m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam exprPl =
    do
        convParams <- convertLamParams lam exprPl
        func <-
            makeFunction
            (lam ^. V.lamParamId & Anchors.assocScopeRef)
            convParams (lam ^. V.lamResult)
        let paramNames =
                func ^.. fParams . _Params . traverse . fpInfo . piTag . tagInfo . tagName
                & Set.fromList
        let lambda
                | useNormalLambda paramNames func =
                    Lambda NormalBinder UnlimitedFuncApply func
                | otherwise =
                    func
                    & SugarLens.funcExprs %~ markLightParams paramNames
                    & Lambda LightLambda UnlimitedFuncApply
        BodyLam lambda
            & addActions lam exprPl
            <&> _PNode . val . SugarLens.bodyChildPayloads .
                pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)

useNormalLambda :: Set InternalName -> Function InternalName i o a -> Bool
useNormalLambda paramNames func
    | Set.size paramNames < 2 = True
    | otherwise =
        any (func &)
        [ Lens.has (fBody . _BinderLet)
        , Lens.has (fBody . SugarLens.binderExprs . SugarLens.payloadsOf forbiddenLightLamSubExprs)
        , not . allParamsUsed paramNames
        ]
    where
        forbiddenLightLamSubExprs :: Lens.Traversal' (Body name i o a) ()
        forbiddenLightLamSubExprs =
            Lens.failing SugarLens.bodyUnfinished
            (_BodyLam . lamFunc . fParams . _Params . Lens.united)

allParamsUsed :: Set InternalName -> Function InternalName i o a -> Bool
allParamsUsed paramNames func =
    Set.null (paramNames `Set.difference` usedParams)
    where
        usedParams =
            func ^.. SugarLens.funcExprs . SugarLens.exprPayloads . Lens.asIndex .
            SugarLens._OfExpr . _BodyGetVar . _GetParam . pNameRef . nrName
            & Set.fromList

markLightParams ::
    Set InternalName -> Expression InternalName (T m) (T m) a ->
    Expression InternalName (T m) (T m) a
markLightParams paramNames (PNode (Node pl bod)) =
    case bod of
    BodyGetVar (GetParam n)
        | Set.member (n ^. pNameRef . nrName) paramNames ->
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    BodyFragment w -> w & fExpr %~ markLightParams paramNames & BodyFragment
    _ -> SugarLens.overBodyChildren id id id (markLightParams paramNames) bod
    & Node pl & PNode

-- Let-item or definition (form of <name> [params] = <body>)
convertAssignment ::
    (Monad m, Monoid a) =>
    BinderKind m -> V.Var -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Assignment InternalName (T m) (T m) (ConvertPayload m a)
    )
convertAssignment binderKind defVar expr =
    do
        (mPresentationModeProp, convParams, funcBody) <-
            convertParams binderKind defVar expr
        makeAssignment (Anchors.assocScopeRef defVar) convParams
            funcBody (expr ^. Val.payload)
            <&> (,) mPresentationModeProp

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Assignment InternalName (T m) (T m) (ConvertPayload m a)
    )
convertDefinitionBinder defI =
    convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI)
