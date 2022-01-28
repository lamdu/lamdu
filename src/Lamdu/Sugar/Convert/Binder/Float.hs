{-# LANGUAGE GADTs, TypeApplications #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax (FuncType(..))
import           Hyper.Syntax.Row (FlatRowExtends(..))
import           Hyper.Type.Prune (Prune)
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Definition as Def
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Binder.Params as Params
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.PostProcess (makeScheme)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

moveToGlobalScope ::
    Monad m => ConvertM m (V.Var -> Definition.Expr (HRef m # V.Term) -> T m ())
moveToGlobalScope =
    (,,)
    <$> Lens.view id
    <*> ConvertM.postProcessAssert
    <*> ConvertM.cachedFunc Cache.infer
    <&>
    \(ctx, postProcess, infer) param defExpr ->
    do
        inferRes <-
            traverse ExprLoad.expr defExpr
            <&> (Load.inferDef infer (ctx ^. ConvertM.scDebugMonitors) ?? param)
        scheme <-
            case inferRes >>= makeScheme of
            Left err -> error ("extract to global scope failed inference: " ++ show (prettyShow err))
            Right x -> pure x
        let defExprI = defExpr <&> (^. ExprIRef.iref)
        DataOps.newPublicDefinitionToIRef
            (ctx ^. Anchors.codeAnchors)
            (Definition.Definition (Definition.BodyExpr defExprI) scheme ())
            (ExprIRef.defI param)
        Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
            (Def.depsGlobalTypes . Lens.at param ?~ scheme)
        -- Prune outer def's deps (some used only in inner) and update
        -- our type which may become generalized due to
        -- extraction/generalization of the inner type
        postProcess

isVarAlwaysApplied :: V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann a -> Bool
isVarAlwaysApplied (V.TypedLam var _paramTyp x) =
    go False x
    where
        go isApplied (Ann _ (V.BLeaf (V.LVar v))) | v == var = isApplied
        go _ (Ann _ (V.BApp (V.App f a))) = go True f && go False a
        go _ v =
            hfoldMap @_ @[Bool]
            ( \case
                HWitness V.W_Term_Term -> (:[]) . go False
                _ -> const []
            ) (v ^. hVal)
            & and

convertLetToLam ::
    Monad m =>
    V.Var ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) ->
    Ann (HRef m) # V.Term ->
    T m (HRef m # V.Term)
convertLetToLam var lam val =
    do
        (newParam, newHRef) <-
            Params.convertBinderToFunction mkArg
            (BinderKindLet lam) val
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValI (prop ^. ExprIRef.iref)
        SubExprs.onGetVars toNewParam var val
        pure newHRef
    where
        mkArg = V.LVar var & V.BLeaf & ExprIRef.newValI

convertVarToGetFieldParam ::
    Monad m =>
    V.Var -> T.Tag -> V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) -> T m ()
convertVarToGetFieldParam oldVar paramTag (V.TypedLam lamVar _paramTyp lamBody) =
    SubExprs.onGetVars toNewParam oldVar lamBody
    where
        toNewParam prop =
            V.App
            <$> ExprIRef.newValI (V.BLeaf (V.LGetField paramTag))
            <*> ExprIRef.newValI (V.BLeaf (V.LVar lamVar))
            <&> V.BApp
            >>= ExprIRef.writeValI (prop ^. ExprIRef.iref)

convertLetParamToRecord ::
    Monad m =>
    V.Var -> V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) -> Params.StoredLam m ->
    ConvertM m (T m (HRef m # V.Term))
convertLetParamToRecord var letLam storedLam =
    Params.convertToRecordParams <&> \toRecordParams ->
    do
        tagForExistingParam <-
            storedLam ^. Params.slLam . V.tlIn & Anchors.assocTag & Property.getP
        addAsTag <-
            Property.getP (Anchors.assocTag var)
            >>=
            \x ->
            if x == tagForExistingParam || x == Anchors.anonTag
            then DataOps.genNewTag
            else pure x
        toRecordParams mkNewArg (BinderKindLet letLam) storedLam Params.NewParamAfter addAsTag
        convertVarToGetFieldParam var addAsTag (storedLam ^. Params.slLam)
        storedLam ^. Params.slLambdaProp & pure
    where
        mkNewArg = V.LVar var & V.BLeaf & ExprIRef.newValI

addFieldToLetLhsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) ->
    Params.StoredLam m -> ConvertM m (T m (HRef m # V.Term))
addFieldToLetLhsRecord fieldTags var letLam storedLam =
    Params.addFieldParam <&>
    \addParam ->
    do
        paramTag <- DataOps.genNewTag
        addParam mkNewArg (BinderKindLet letLam) storedLam
            ((fieldTags ++) . pure) paramTag
        convertVarToGetFieldParam var paramTag (storedLam ^. Params.slLam)
        storedLam ^. Params.slLambdaProp & pure
    where
        mkNewArg = V.LVar var & V.BLeaf & ExprIRef.newValI

addLetParam ::
    Monad m =>
    V.Var ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (T m (HRef m # V.Term))
addLetParam var lam val =
    case valStored ^. hVal of
    V.BLam innerLam | isVarAlwaysApplied lam ->
        case val ^. hAnn . Input.inferredType . _Pure of
        T.TFun (FuncType (Pure (T.TRecord composite)) _)
            | FlatRowExtends fieldsMap (Pure T.REmpty) <- composite ^. T.flatRow
            , Params.isParamAlwaysUsedWithGetField innerLam ->
            addFieldToLetLhsRecord (fieldsMap ^.. Lens.itraversed . Lens.asIndex)
            var lam storedLam
        _ -> convertLetParamToRecord var lam storedLam
        where
            storedLam = Params.StoredLam innerLam (val ^. hAnn . Input.stored)
    _ -> convertLetToLam var lam valStored & pure
    where
        valStored = val & hflipped %~ hmap (const (^. Input.stored))

{-# ANN ordNub ("HLint: ignore Use ^.."::String) #-}
ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

processLet ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (T m (HRef m # V.Term))
processLet lam val =
    do
        scopeInfo <- Lens.view ConvertM.scScopeInfo
        let usedLocalVars =
                val ^.. ExprLens.valLeafs . V._LVar
                & ordNub
                & filter (\x -> innerScopeLocalVars ^. Lens.contains x)
        let varsExitingScope =
                case scopeInfo ^. ConvertM.siFloatPos of
                Nothing -> usedLocalVars
                Just outerScopeInfo ->
                    filter (\x -> Lens.hasn't (Lens.ix x) outerScope) usedLocalVars
                    where
                        outerScope = outerScopeInfo ^. ConvertM.osiScope . V.scopeVarTypes
        case varsExitingScope of
            [] -> val ^. hAnn . Input.stored & pure & pure
            [x] -> addLetParam x lam val
            _ -> error "multiple osiVarsUnderPos not expected!?"
    where
        innerScopeLocalVars = val ^. hAnn . Input.localsInScope <&> fst & Set.fromList

makeFloatLetToOuterScope ::
    Monad m =>
    (ValI m -> T m ()) ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (T m ExtractDestination)
makeFloatLetToOuterScope setTopLevel lam val =
    (,,,)
    <$> processLet newLam val
    <*> Lens.view id
    <*> moveToGlobalScope
    <*> ConvertM.postProcessWith
    <&>
    \(makeNewLet, ctx, floatToGlobal, postProcess) ->
    do
        lam ^. V.tlOut . hAnn . ExprIRef.iref & setTopLevel
        newLetP <- makeNewLet
        r <-
            case ctx ^. ConvertM.scScopeInfo . ConvertM.siFloatPos of
            Nothing ->
                EntityId.ofIRef (ExprIRef.defI param) <$
                floatToGlobal param innerDefExpr <&> ExtractToDef
                where
                    addRecursiveRefAsDep =
                        case ctx ^. ConvertM.scScopeInfo . ConvertM.siRecursiveRef of
                        Nothing -> id
                        Just (ConvertM.RecursiveRef defI defType) ->
                            Def.depsGlobalTypes . Lens.at (ExprIRef.globalId defI) ?~ defType
                    innerDefExpr = Definition.Expr newLetP innerDeps
                    innerDeps =
                        -- Outer deps, pruned:
                        ctx ^. ConvertM.scFrozenDeps . Property.pVal
                        & addRecursiveRefAsDep
                        & Def.pruneDeps val
            Just outerScopeInfo ->
                EntityId.ofValI (val ^. hAnn . Input.stored . ExprIRef.iref) <$
                DataOps.redexWrapWithGivenParam param
                (newLetP ^. ExprIRef.iref) (outerScopeInfo ^. ConvertM.osiPos)
                <&> ExtractToLet
        r <$ postProcess fixUsages
    where
        param = lam ^. V.tlIn
        fixUsages _ =
            Load.readValAndAddProperties (newLam ^. V.tlOut . hAnn)
            >>= SubExprs.onGetVars (void . DataOps.applyHoleTo) param
        newLam = lam & V.tlOut . hAnn . ExprIRef.setIref .~ setTopLevel
