{-# LANGUAGE DisambiguateRecordFields, TupleSections #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens.Extended as Lens
import qualified Data.Map as Map
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.Tree.Diverse (Node(..), Ann(..), _Node, ann, val, annotations)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
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
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeActions, subexprPayloads)
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
    SubExprs.getVarsToHole param (x & annotations %~ (^. Input.stored))

localNewExtractDestPos ::
    Val (Input.Payload m x) -> ConvertM m a -> ConvertM m a
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. _Node . ann . Input.stored
    , _osiScope = x ^. _Node . ann . Input.inferred . Infer.plScope
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

convertRedex ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    Redex (Input.Payload m a) ->
    ConvertM m (Let InternalName (T m) (T m) (Ann (ConvertPayload m a)))
convertRedex expr redex =
    do
        tag <- convertTaggedEntity param
        (_pMode, value) <-
            convertAssignment binderKind param (redex ^. Redex.arg)
            & localNewExtractDestPos expr
            <&> _2 . _Node . ann . pInput . Input.entityId .~
                EntityId.ofValI (redex ^. Redex.arg . _Node . ann . Input.stored . Property.pVal)
        letBody <-
            convertBinder bod
            & ConvertM.local (scScopeInfo . siLetItems <>~
                Map.singleton param (makeInline stored redex))
        float <- makeFloatLetToOuterScope (stored ^. Property.pSet) redex
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . _Node . ann . Input.stored . Property.pVal)
                        <&> EntityId.ofValI
                    )
        postProcess <- ConvertM.postProcessAssert
        let del =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.lamResult . _Node . ann . Input.stored
                        & replaceWith stored & void
                <* postProcess
        pure Let
            { _lVarInfo = redex ^. Redex.arg . _Node . ann . Input.inferred . Infer.plType & mkVarInfo
            , _lValue = value & _Node . ann . pActions %~ fixValueNodeActions
            , _lDelete = del
            , _lName = tag
            , _lBodyScope = redex ^. Redex.bodyScope
            , _lBody =
                letBody
                & _Node . ann . pActions . mReplaceParent ?~
                    (letBody ^. _Node . ann . pInput . Input.entityId <$ del)
            , _lUsages = redex ^. Redex.paramRefs
            }
    where
        stored = expr ^. _Node . ann . Input.stored
        binderKind =
            redex ^. Redex.lam
            <&> annotations %~ (^. Input.stored)
            & BinderKindLet
        V.Lam param bod = redex ^. Redex.lam

convertBinderBody ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (Binder InternalName (T m) (T m) (Ann (ConvertPayload m a)), a)
convertBinderBody expr =
    case Redex.check expr of
    Nothing ->
        ConvertM.convertSubexpression expr
        & localNewExtractDestPos expr
        <&> (^. _Node . val)
        <&>
        \body ->
        ( BinderExpr body
        , subexprPayloads
            (expr ^.. _Node . val . V.termChildren)
            (body ^.. SugarLens.bodyChildPayloads)
            & mconcat
        )
    Just redex ->
        convertRedex expr redex
        <&>
        \l ->
        ( BinderLet l
        , redex ^. Redex.lamPl . Input.userData
        )

convertBinder ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (Node (Ann (ConvertPayload m a)) (Binder InternalName (T m) (T m)))
convertBinder expr =
    do
        actions <-
            makeActions (expr ^. _Node . ann) & localNewExtractDestPos expr
        (b, hiddenPls) <- convertBinderBody expr
        Node Ann
            { _val = b
            , _ann =
                ConvertPayload
                { _pInput = expr ^. _Node . ann & Input.userData .~ hiddenPls
                , _pActions = actions
                }
            } & pure

makeFunction ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m (Function InternalName (T m) (T m) (Ann (ConvertPayload m a)))
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
        (bod, hiddenEntityIds) <-
            case params ^. cpParams of
            Nothing ->
                convertBinderBody funcBody
                <&> _1 %~ BodyPlain . AssignPlain (params ^. cpAddFirstParam)
            Just{} ->
                makeFunction chosenScopeProp params funcBody <&> BodyFunction <&> (, mempty)
        nodeActions <- makeActions pl
        let mRemoveSetToHole
                | Lens.has (_BodyPlain . apBody . _BinderExpr . _BodyHole) bod =
                    mSetToHole .~ Nothing
                | otherwise = id
        Node Ann
            { _ann =
                ConvertPayload
                { _pInput = pl & Input.userData .~ hiddenEntityIds
                , _pActions = mRemoveSetToHole nodeActions
                }
            , _val = bod
            } & pure

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
                    & fBody %~ markBinderLightParams paramNames
                    & Lambda LightLambda UnlimitedFuncApply
        BodyLam lambda
            & addActions lam exprPl
            <&> _Node . val . SugarLens.bodyChildPayloads .
                pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)

useNormalLambda :: Set InternalName -> Function InternalName i o (Ann a) -> Bool
useNormalLambda paramNames func
    | Set.size paramNames < 2 = True
    | otherwise =
        any (func &)
        [ Lens.has (fBody . _Node . val . _BinderLet)
        , Lens.has (fBody . SugarLens.binderPayloads . Lens.filteredByIndex
            (SugarLens._OfExpr . forbiddenLightLamSubExprs))
        , not . allParamsUsed paramNames
        ]
    where
        forbiddenLightLamSubExprs :: Lens.Traversal' (Body name i o (Ann a)) ()
        forbiddenLightLamSubExprs =
            Lens.failing SugarLens.bodyUnfinished
            (_BodyLam . lamFunc . fParams . _Params . Lens.united)

allParamsUsed :: Set InternalName -> Function InternalName i o (Ann a) -> Bool
allParamsUsed paramNames func =
    Set.null (paramNames `Set.difference` usedParams)
    where
        usedParams =
            func ^.. fBody . SugarLens.binderPayloads . Lens.asIndex .
            SugarLens._OfExpr . _BodyGetVar . _GetParam . pNameRef . nrName
            & Set.fromList

markBinderLightParams ::
    Set InternalName ->
    Node (Ann a) (Binder InternalName (T m) (T m)) ->
    Node (Ann a) (Binder InternalName (T m) (T m))
markBinderLightParams paramNames (Node (Ann pl bod)) =
    SugarLens.overBinderChildren id id id
    (markElseLightParams paramNames) (markBinderLightParams paramNames)
    (markLightParams paramNames) fixAssignments
    bod
    & Ann pl & Node
    where
        fixAssignments =
            -- No assignments inside light lambdas. Consider asserting?
            id

markElseLightParams ::
    Set InternalName ->
    Node (Ann a) (Else InternalName (T m) (T m)) ->
    Node (Ann a) (Else InternalName (T m) (T m))
markElseLightParams paramNames =
    _Node . val %~
    \case
    SimpleElse body -> markBodyLightParams paramNames body & SimpleElse
    ElseIf elseIf ->
        elseIf
        & eiContent %~
            SugarLens.overIfElseChildren
            (markElseLightParams paramNames)
            (markLightParams paramNames)
        & ElseIf

markLightParams ::
    Set InternalName ->
    Expression InternalName (T m) (T m) a ->
    Expression InternalName (T m) (T m) a
markLightParams paramNames = _Node . val %~ markBodyLightParams paramNames

markBodyLightParams ::
    Set InternalName ->
    Body InternalName (T m) (T m) (Ann a) ->
    Body InternalName (T m) (T m) (Ann a)
markBodyLightParams paramNames =
    \case
    BodyGetVar (GetParam n)
        | Set.member (n ^. pNameRef . nrName) paramNames ->
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    BodyFragment w -> w & fExpr %~ markLightParams paramNames & BodyFragment
    bod ->
        SugarLens.overBodyChildren id id id (markElseLightParams paramNames)
        (markBinderLightParams paramNames) (markLightParams paramNames) bod

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
            funcBody (expr ^. _Node . ann)
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
