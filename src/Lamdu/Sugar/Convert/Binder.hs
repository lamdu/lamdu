{-# LANGUAGE TypeApplications, DisambiguateRecordFields, KindSignatures, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses, DataKinds #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens.Extended as Lens
import           Data.Constraint (Dict(..))
import qualified Data.Map as Map
import           Data.Monoid (Any(..))
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Recurse (Recursive(..), foldMapRecursive, (##>>))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
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
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    Tree (V.Lam V.Var V.Term) (Ann (Const (Input.Payload m a))) -> T m ()
lamParamToHole (V.Lam param x) =
    SubExprs.getVarsToHole param (x & Lens.from _HFlip . hmapped1 %~ (^. Lens._Wrapped . Input.stored))

makeInline ::
    Monad m =>
    Tree (HRef m) V.Term -> Tree Redex (Const (Input.Payload m a)) -> EntityId -> BinderVarInline (T m)
makeInline stored redex useId
    | Lens.has traverse otherUses = CannotInlineDueToUses (drop 1 after ++ before)
    | otherwise =
        inlineLet stored (redex & hmapped1 %~ (^. Lens._Wrapped . Input.stored . ExprIRef.iref))
        & InlineVar
    where
        otherUses = filter (/= useId) uses
        uses = redex ^. Redex.paramRefs
        (before, after) = break (== useId) uses

convertLet ::
    (Monad m, Monoid a) =>
    Input.Payload m a ->
    Tree Redex (Const (Input.Payload m a)) ->
    ConvertM m
    (Annotated (ConvertPayload m a) (Binder InternalName (T m) (T m)))
convertLet pl redex =
    do
        float <- makeFloatLetToOuterScope (pl ^. Input.stored . ExprIRef.setIref) redex
        tag <- ConvertTag.taggedEntity param
        (value, letBody, actions) <-
            do
                (_pMode, value) <-
                    convertAssignment binderKind param (redex ^. Redex.arg)
                    <&> _2 . annotation . pInput . Input.entityId .~
                        EntityId.ofValI (redex ^. Redex.arg . annotation . Input.stored . ExprIRef.iref)
                letBody <-
                    convertBinder bod
                    & ConvertM.local (scScopeInfo . siLetItems <>~
                        Map.singleton param (makeInline stored redex))
                actions <- makeActions pl
                pure (value, letBody, actions)
            & localNewExtractDestPos pl
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . annotation . Input.stored . ExprIRef.iref)
                        <&> EntityId.ofValI
                    )
        postProcess <- ConvertM.postProcessAssert
        let del =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.lamOut . annotation . Input.stored
                        & replaceWith stored & void
                <* postProcess
        typS <-
            convertType (EntityId.ofTypeOf (argAnn ^. Input.entityId))
            (argAnn ^. Input.inferredType)
        pure Ann
            { _hVal =
                BinderLet Let
                { _lVarInfo = mkVarInfo typS
                , _lValue = value & annotation . pActions %~ fixValueNodeActions
                , _lDelete = del
                , _lName = tag
                , _lBodyScope = redex ^. Redex.bodyScope
                , _lBody =
                    letBody
                    & annotation . pActions . mReplaceParent ?~
                        (letBody ^. annotation . pInput . Input.entityId <$ del)
                , _lUsages = redex ^. Redex.paramRefs
                }
            , _hAnn =
                Const ConvertPayload
                { _pInput =
                    pl
                    & Input.userData .~ redex ^. Redex.lamPl . Lens._Wrapped . Input.userData
                , _pActions = actions
                }
            }
    where
        argAnn = redex ^. Redex.arg . annotation
        stored = pl ^. Input.stored
        binderKind =
            redex ^. Redex.lam
            & V.lamOut . Lens.from _HFlip . hmapped1 %~ (^. Lens._Wrapped . Input.stored)
            & BinderKindLet
        V.Lam param bod = redex ^. Redex.lam

convertBinder ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (Annotated (ConvertPayload m a) (Binder InternalName (T m) (T m)))
convertBinder expr@(Ann (Const pl) body) =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.letExpression) >>=
    \case
    False -> convertExpr
    True ->
        case Redex.check body of
        Nothing -> convertExpr
        Just redex -> convertLet pl redex
    where
        convertExpr =
            do
                convertSub <- Lens.view (Lens.to ConvertM.scConvertSubexpression)
                convertSub ConvertM.BinderPos expr
            & localNewExtractDestPos pl
            <&>
            \(Ann (Const a) x) ->
            Ann
            { _hAnn =
                a & pInput .~
                ( pl & Input.userData .~
                    mconcat
                    (subexprPayloads
                    (body ^.. htraverse1)
                    (x ^.. SugarLens.childPayloads))
                )
                & Const
            , _hVal = BinderExpr x
            }

localNewExtractDestPos :: Input.Payload m a -> ConvertM m b -> ConvertM m b
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. Input.stored
    , _osiScope = x ^. Input.inferScope
    }
    & ConvertM.local

makeFunction ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m
    (Tree (Function InternalName (T m) (T m)) (Ann (Const (ConvertPayload m a))))
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
    ConvertM m
    (Annotated (ConvertPayload m a) (Assignment InternalName (T m) (T m)))
makeAssignment chosenScopeProp params funcBody pl =
    case params ^. cpParams of
    Nothing ->
        convertBinder funcBody
        <&>
        \(Ann (Const a) x) ->
        AssignPlain (params ^. cpAddFirstParam) x
        & BodyPlain
        & Ann (Const a)
    Just{} ->
        do
            funcS <- makeFunction chosenScopeProp params funcBody
            nodeActions <- makeActions pl & localNewExtractDestPos pl
            pure Ann
                { _hAnn =
                    Const ConvertPayload
                    { _pInput =
                        -- TODO: Why are redundant hidden entity ids
                        -- returned here?
                        pl & Input.userData .~ mempty
                    , _pActions = nodeActions
                    }
                , _hVal = BodyFunction funcS
                }

convertLam ::
    (Monad m, Monoid a) =>
    Annotated (Input.Payload m a) (V.Lam V.Var V.Term) ->
    ConvertM m (ExpressionU m a)
convertLam (Ann (Const exprPl) lam) =
    do
        convParams <- convertLamParams lam exprPl
        func <-
            makeFunction
            (lam ^. V.lamIn & Anchors.assocScopeRef)
            convParams (lam ^. V.lamOut)
        let paramNames =
                func ^.. fParams . _Params . traverse . fpInfo . piTag . tagRefTag . tagName
                & Set.fromList
        lightLamSugar <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.lightLambda)
        let lambda
                | useNormalLambda paramNames func || not lightLamSugar =
                    Lambda NormalBinder UnlimitedFuncApply func
                | otherwise =
                    func
                    & fBody %~ markNodeLightParams paramNames
                    & Lambda LightLambda UnlimitedFuncApply
        BodyLam lambda
            & addActions (lam ^.. V.lamOut) exprPl
            <&> hVal %~
                hmap (const (annotation . pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)))

useNormalLambda ::
    Set InternalName -> Tree (Function InternalName i o) (Ann (Const a)) -> Bool
useNormalLambda paramNames func
    | Set.size paramNames < 2 = True
    | otherwise =
        ( foldMapRecursive
            (Proxy @SugarLens.SugarExpr ##>>
                Any . SugarLens.isForbiddenInLightLam
            ) (func ^. fBody . hVal)
            ^. Lens._Wrapped
        ) || not (allParamsUsed paramNames func)

class GetParam (t :: AHyperType -> *) where
    getParam :: t f -> Maybe InternalName
    getParam _ = Nothing

    getParamRecursive ::
        Proxy t -> Dict (HNodesConstraint t GetParam)
    default getParamRecursive ::
        HNodesConstraint t GetParam =>
        Proxy t -> Dict (HNodesConstraint t GetParam)
    getParamRecursive _ = Dict

instance Recursive GetParam where
    recurse =
        getParamRecursive . p
        where
            p :: Proxy (GetParam k) -> Proxy k
            p _ = Proxy

instance GetParam (Const (BinderVarRef InternalName o)) where
instance GetParam (Const (NullaryVal InternalName i o))

instance GetParam (Else InternalName i o)

instance GetParam (Function InternalName i o) where

instance GetParam (Const (GetVar InternalName o)) where
    getParam = (^? Lens._Wrapped . _GetParam . pNameRef . nrName)

instance GetParam (Assignment InternalName i o) where
    getParam x = x ^? _BodyPlain . apBody >>= getParam

instance GetParam (Binder InternalName i o) where
    getParam x = x ^? _BinderExpr >>= getParam

instance GetParam (Body InternalName i o) where
    getParam x = x ^? _BodyGetVar <&> Const >>= getParam

allParamsUsed ::
    Set InternalName -> Tree (Function InternalName i o) (Ann (Const a)) -> Bool
allParamsUsed paramNames func =
    Set.null (paramNames `Set.difference` usedParams)
    where
        usedParams =
            foldMapRecursive
            ( Proxy @GetParam ##>>
                (^. Lens._Just . Lens.to Set.singleton) . getParam
            ) func

class MarkLightParams (t :: AHyperType -> *) where
    markLightParams :: Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)

    default markLightParams ::
        (HFunctor t, HNodesConstraint t MarkLightParams) =>
        Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)
    markLightParams = defaultMarkLightParams

defaultMarkLightParams ::
    (HFunctor t, HNodesConstraint t MarkLightParams) =>
    Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)
defaultMarkLightParams paramNames =
    hmap (Proxy @MarkLightParams #> markNodeLightParams paramNames)

markNodeLightParams ::
    MarkLightParams t =>
    Set InternalName ->
    Tree (Ann a) t ->
    Tree (Ann a) t
markNodeLightParams paramNames =
    hVal %~ markLightParams paramNames

instance MarkLightParams (Lens.Const a)
instance MarkLightParams (Else InternalName i o)
instance MarkLightParams (Let InternalName i o)
instance MarkLightParams (Function InternalName i o)

instance MarkLightParams (Assignment InternalName i o) where
    markLightParams ps (BodyPlain x) = x & apBody %~ markLightParams ps & BodyPlain
    markLightParams ps (BodyFunction x) = markLightParams ps x & BodyFunction

instance MarkLightParams (Binder InternalName i o) where
    markLightParams ps (BinderExpr x) = markLightParams ps x & BinderExpr
    markLightParams ps (BinderLet x) = markLightParams ps x & BinderLet

instance MarkLightParams (Body InternalName i o) where
    markLightParams paramNames (BodyGetVar (GetParam n))
        | paramNames ^. Lens.contains (n ^. pNameRef . nrName) =
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    markLightParams paramNames bod = defaultMarkLightParams paramNames bod

-- Let-item or definition (form of <name> [params] = <body>)
convertAssignment ::
    (Monad m, Monoid a) =>
    BinderKind m -> V.Var -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m a) (Assignment InternalName (T m) (T m))
    )
convertAssignment binderKind defVar expr =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.assignmentParameters)
    >>=
    \case
    False ->
        convertBinder expr
        <&>
        \(Ann (Const a) v) ->
        ( Nothing
        , AssignPlain (AddInitialParam (error "TODO: add param when assignment parameters not supported")) v
            & BodyPlain
            & Ann (Const a)
        )
    True ->
        do
            (mPresentationModeProp, convParams, funcBody) <-
                convertParams binderKind defVar expr
            makeAssignment (Anchors.assocScopeRef defVar) convParams
                funcBody (expr ^. annotation)
                <&> (,) mPresentationModeProp

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m a) (Assignment InternalName (T m) (T m))
    )
convertDefinitionBinder defI =
    convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI)
