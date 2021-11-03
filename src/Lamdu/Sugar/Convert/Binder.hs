{-# LANGUAGE TypeApplications, DisambiguateRecordFields, FlexibleInstances, DefaultSignatures, DataKinds, TypeFamilies #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import qualified Data.Map as Map
import           Data.Monoid (Any(..))
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Recurse (Recursive(..), foldMapRecursive, proxyArgument, (##>>))
import           Hyper.Type.Prune (Prune)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.UniqueId (ToUUID(..))
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertLamParams, convertEmptyParams, convertNonEmptyParams, cpParams, cpAddFirstParam, mkVarInfo)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..), scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m a) -> T m ()
lamParamToHole (V.TypedLam param _paramTyp x) =
    SubExprs.getVarsToHole param (x & hflipped %~ hmap (const (^. Input.stored)))

makeInline ::
    Monad m =>
    HRef m # V.Term -> Redex # Input.Payload m a -> EntityId -> BinderVarInline (T m)
makeInline stored redex useId
    | Lens.has traverse otherUses = CannotInlineDueToUses (drop 1 after <> before)
    | otherwise =
        inlineLet stored (Redex.hmapRedex (const (^. Input.stored . ExprIRef.iref)) redex)
        & InlineVar
    where
        otherUses = filter (/= useId) uses
        uses = redex ^. Redex.paramRefs
        (before, after) = break (== useId) uses

convertLet ::
    (Monad m, Monoid a) =>
    Input.Payload m a # V.Term ->
    Redex # Input.Payload m a ->
    ConvertM m (Annotated (ConvertPayload m a) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertLet pl redex =
    do
        float <- makeFloatLetToOuterScope (pl ^. Input.stored . ExprIRef.setIref) redex
        vinfo <- mkVarInfo (argAnn ^. Input.inferredType)
        tag <- ConvertTag.taggedEntity (Just vinfo) param >>= ConvertM . lift
        (value, letBody, actions) <-
            (,,)
            <$> (convertAssignment binderKind param (redex ^. Redex.arg) <&> (^. _2))
            <*> ( convertBinder bod
                    & local (scScopeInfo . siLetItems <>~
                        Map.singleton param (makeInline stored redex))
                )
            <*> makeActions pl
            & localNewExtractDestPos pl
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . hAnn . Input.stored . ExprIRef.iref)
                        <&> EntityId.ofValI
                    )
        postProcess <- ConvertM.postProcessAssert
        let del =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.tlOut . hAnn . Input.stored
                        & replaceWith stored & void
                <* postProcess
        pure Ann
            { _hVal =
                BinderLet Let
                { _lValue = value & annotation . pActions %~ fixValueNodeActions
                , _lDelete = del
                , _lName = tag
                , _lBody =
                    letBody
                    & annotation . pActions . mReplaceParent ?~
                        (letBody ^. annotation . pInput . Input.entityId <$ del)
                    & annotation . pLambdas .~ [redex ^. Redex.lamPl . Input.stored . ExprIRef.iref & toUUID]
                , _lUsages = redex ^. Redex.paramRefs
                }
            , _hAnn =
                Const ConvertPayload
                { _pInput =
                    pl
                    & Input.userData .~
                        redex ^. Redex.lamPl . Input.userData <>
                        hfoldMap (const (^. Input.userData)) (redex ^. Redex.lam . V.tlInType . hflipped)
                , _pActions = actions
                , _pLambdas = []
                }
            }
    where
        argAnn = redex ^. Redex.arg . hAnn
        stored = pl ^. Input.stored
        binderKind =
            redex ^. Redex.lam
            & hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (const (^. Input.stored)))
            & BinderKindLet
        V.TypedLam param _paramTyp bod = redex ^. Redex.lam

convertBinder ::
    (Monad m, Monoid a) =>
    Ann (Input.Payload m a) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m a) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertBinder expr@(Ann pl body) =
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
                convertSub <- Lens.view id <&> \env -> ConvertM.scConvertSubexpression env
                convertSub ConvertM.BinderPos expr
            & localNewExtractDestPos pl
            <&> annValue %~ BinderTerm

localNewExtractDestPos ::
    Monad m => Input.Payload m a # V.Term -> ConvertM m b -> ConvertM m b
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. Input.stored
    , _osiScope = x ^. Input.inferScope
    }
    & local

makeFunction ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Ann (Input.Payload m a) # V.Term ->
    ConvertM m (Function EvalPrep InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a))
makeFunction chosenScopeProp params funcBody =
    convertBinder funcBody
    <&> mkRes
    & local (ConvertM.scScopeInfo %~ addParams)
    where
        mkRes assignmentBody =
            Function
            { _fParams =
                -- TODO: avoid partiality here
                params ^?! cpParams . Lens._Just
            , _fChosenScopeProp = chosenScopeProp ^. Property.mkProperty & lift
            , _fBody = assignmentBody
            , _fBodyScopes = mempty
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
    BinderKind m -> V.Var -> Ann (Input.Payload m a) # V.Term ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m a) # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
makeAssignment chosenScopeProp binderKind defVar (Ann pl (V.BLam lam)) =
    do
        convParams <- convertNonEmptyParams (Just presMode) binderKind lam pl
        funcS <- makeFunction chosenScopeProp convParams (lam ^. V.tlOut)
        nodeActions <- makeActions pl & localNewExtractDestPos pl
        pure
            ( presMode <$ convParams ^? cpParams . Lens._Just . _RecordParams . tlItems . Lens._Just . tlTail . traverse
            , Ann
                { _hAnn =
                    Const ConvertPayload
                    { _pInput =
                        pl & Input.userData .~
                        hfoldMap (const (^. Input.userData)) (lam ^. V.tlInType . hflipped)
                    , _pActions = nodeActions
                    , _pLambdas = []
                    }
                , _hVal = BodyFunction funcS
                }
            )
    where
        presMode = Anchors.assocPresentationMode defVar
makeAssignment _chosenScopeProp binderKind _defVar expr =
    do
        addFirstParam <- convertEmptyParams binderKind expr
        convertBinder expr
            <&> annValue %~ BodyPlain . AssignPlain (AddInitialParam addFirstParam)
    <&> (,) Nothing

convertLam ::
    (Monad m, Monoid a) =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convertLam lam exprPl =
    do
        convParams <- convertLamParams lam exprPl
        func <-
            makeFunction
            (lam ^. V.tlIn & Anchors.assocScopeRef)
            convParams (lam ^. V.tlOut)
        let paramNames =
                func ^..
                fParams .
                (_RecordParams . SugarLens.taggedListItems . tiTag <> _VarParam . _2 . vpiTag . oTag) .
                tagRefTag . tagName
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
            & addActions lam exprPl
            <&> hVal %~
                hmap (const (annotation . pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)))

useNormalLambda ::
    Set InternalName ->
    Function v InternalName i o # Annotated a -> Bool
useNormalLambda paramNames func =
    foldMapRecursive
    ( Proxy @SugarLens.SugarExpr ##>>
        Any . SugarLens.isForbiddenInLightLam
    ) (func ^. fBody . hVal) ^. Lens._Wrapped
    || paramsCond
    where
        paramsCond = not allParamsUsed && not (null usedParams)
        allParamsUsed = Set.null (paramNames `Set.difference` usedParams)
        usedParams =
            foldMapRecursive
            ( Proxy @GetParam ##>>
                (^. Lens._Just . Lens.to Set.singleton) . getParam
            ) func

class GetParam t where
    getParam :: t f -> Maybe InternalName
    getParam _ = Nothing

    getParamRecursive ::
        Proxy t -> Dict (HNodesConstraint t GetParam)
    default getParamRecursive ::
        HNodesConstraint t GetParam =>
        Proxy t -> Dict (HNodesConstraint t GetParam)
    getParamRecursive _ = Dict

instance Recursive GetParam where
    recurse = getParamRecursive . proxyArgument

instance GetParam (Const (BinderVarRef InternalName o))
instance GetParam (Const (i (TagChoice InternalName o)))
instance GetParam (Const (TagRef InternalName i o))
instance GetParam (Const (TId name o))
instance GetParam (Else v InternalName i o)
instance GetParam (Function v InternalName i o)
instance GetParam (PostfixFunc v InternalName i o)

instance GetParam (Const (GetVar InternalName o)) where
    getParam = (^? Lens._Wrapped . _GetParam . pNameRef . nrName)

instance GetParam (Assignment v InternalName i o) where
    getParam x = x ^? _BodyPlain . apBody >>= getParam

instance GetParam (Binder v InternalName i o) where
    getParam x = x ^? _BinderTerm >>= getParam

instance GetParam (Term v InternalName i o) where
    getParam x = x ^? _BodyLeaf . _LeafGetVar <&> Const >>= getParam

class MarkLightParams t where
    markLightParams :: Set InternalName -> t # Ann a -> t # Ann a

    default markLightParams ::
        (HFunctor t, HNodesConstraint t MarkLightParams) =>
        Set InternalName -> t # Ann a -> t # Ann a
    markLightParams = defaultMarkLightParams

defaultMarkLightParams ::
    (HFunctor t, HNodesConstraint t MarkLightParams) =>
    Set InternalName -> t # Ann a -> t # Ann a
defaultMarkLightParams paramNames =
    hmap (Proxy @MarkLightParams #> markNodeLightParams paramNames)

markNodeLightParams ::
    MarkLightParams t =>
    Set InternalName ->
    Ann a # t ->
    Ann a # t
markNodeLightParams paramNames =
    hVal %~ markLightParams paramNames

-- instance MarkLightParams (Const a)
instance MarkLightParams (Const (BinderVarRef InternalName o))
instance MarkLightParams (Const (i (TagChoice InternalName o)))
instance MarkLightParams (Const (TagRef InternalName i o))
instance MarkLightParams (Else v InternalName i o)
instance MarkLightParams (Let v InternalName i o)
instance MarkLightParams (Function v InternalName i o)
instance MarkLightParams (PostfixFunc v InternalName i o)

instance MarkLightParams (Const (GetVar InternalName o)) where
    markLightParams paramNames =
        Lens._Wrapped . _GetParam . Lens.filteredBy (pNameRef . nrName . Lens.filtered f) . pBinderMode .~ LightLambda
        where
            f n = paramNames ^. Lens.contains n

instance MarkLightParams (Assignment v InternalName i o) where
    markLightParams ps (BodyPlain x) = x & apBody %~ markLightParams ps & BodyPlain
    markLightParams ps (BodyFunction x) = markLightParams ps x & BodyFunction

instance MarkLightParams (Binder v InternalName i o) where
    markLightParams ps (BinderTerm x) = markLightParams ps x & BinderTerm
    markLightParams ps (BinderLet x) = markLightParams ps x & BinderLet

instance MarkLightParams (Term v InternalName i o) where
    markLightParams paramNames (BodyLeaf (LeafGetVar x)) =
        markLightParams paramNames (Const x) ^. Lens._Wrapped & LeafGetVar & BodyLeaf
    markLightParams paramNames bod = defaultMarkLightParams paramNames bod

-- Let-item or definition (form of <name> [params] = <body>)
convertAssignment ::
    (Monad m, Monoid a) =>
    BinderKind m -> V.Var ->
    Ann (Input.Payload m a) # V.Term ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m a) # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertAssignment binderKind defVar expr =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.assignmentParameters)
    >>=
    \case
    False ->
        convertBinder expr
        <&> annValue %~ BodyPlain . AssignPlain (AddInitialParam (error "TODO: add param when assignment parameters not supported"))
        <&> (,) Nothing
    True -> makeAssignment (Anchors.assocScopeRef defVar) binderKind defVar expr

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Ann (Input.Payload m a) # V.Term ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m a)
        # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertDefinitionBinder defI =
    convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI)
