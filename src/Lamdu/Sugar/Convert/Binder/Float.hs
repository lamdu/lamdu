{-# LANGUAGE PatternGuards #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import           AST (Tree, Pure(..), monoChildren)
import           AST.Knot.Ann (Ann(..), ann, val)
import           AST.Term.FuncType (FuncType(..))
import           AST.Term.Row (FlatRowExtends(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Definition as Def
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Binder.Params as Params
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
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
    Monad m => ConvertM m (V.Var -> Definition.Expr (ValP m) -> T m ())
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
            >>= (Load.inferDef infer (ctx ^. ConvertM.scDebugMonitors)
                    (pure EvalResults.empty) ?? param)
        scheme <-
            case inferRes >>= makeScheme of
            Left err -> fail ("extract to global scope failed inference: " ++ show (prettyShow err))
            Right x -> pure x
        let defExprI = defExpr <&> Property.value
        DataOps.newPublicDefinitionToIRef
            (ctx ^. ConvertM.scCodeAnchors)
            (Definition.Definition (Definition.BodyExpr defExprI) scheme ())
            (ExprIRef.defI param)
        Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
            (Def.depsGlobalTypes . Lens.at param ?~ scheme)
        -- Prune outer def's deps (some used only in inner) and update
        -- our type which may become generalized due to
        -- extraction/generalization of the inner type
        postProcess

isVarAlwaysApplied :: Tree (V.Lam V.Var V.Term) (Ann a) -> Bool
isVarAlwaysApplied (V.Lam var x) =
    go False x
    where
        go isApplied (Ann _ (V.BLeaf (V.LVar v))) | v == var = isApplied
        go _ (Ann _ (V.BApp (V.Apply f a))) = go True f && go False a
        go _ v = all (go False) (v ^.. val . monoChildren)

convertLetToLam ::
    Monad m => V.Var -> Redex (ValP m) -> T m (ValP m)
convertLetToLam var redex =
    do
        (newParam, newValP) <-
            Params.convertBinderToFunction mkArg
            (BinderKindLet (redex ^. Redex.lam)) (redex ^. Redex.arg)
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValI (Property.value prop)
        SubExprs.onGetVars toNewParam var (redex ^. Redex.arg)
        pure newValP
    where
        mkArg = V.LVar var & V.BLeaf & ExprIRef.newValI

convertVarToGetFieldParam ::
    Monad m =>
    V.Var -> T.Tag -> Tree (V.Lam V.Var V.Term) (Ann (ValP m)) -> T m ()
convertVarToGetFieldParam oldVar paramTag (V.Lam lamVar lamBody) =
    SubExprs.onGetVars toNewParam oldVar lamBody
    where
        toNewParam prop =
            V.LVar lamVar & V.BLeaf
            & ExprIRef.newValI
            <&> (`V.GetField` paramTag) <&> V.BGetField
            >>= ExprIRef.writeValI (Property.value prop)

convertLetParamToRecord ::
    Monad m =>
    V.Var -> Tree (V.Lam V.Var V.Term) (Ann (ValP m)) -> Params.StoredLam m ->
    ConvertM m (T m (ValP m))
convertLetParamToRecord var letLam storedLam =
    Params.convertToRecordParams <&> \toRecordParams ->
    do
        tagForExistingParam <-
            storedLam ^. Params.slLam . V.lamIn & Anchors.assocTag & Property.getP
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

addFieldToLetParamsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> Tree (V.Lam V.Var V.Term) (Ann (ValP m)) ->
    Params.StoredLam m -> ConvertM m (T m (ValP m))
addFieldToLetParamsRecord fieldTags var letLam storedLam =
    Params.addFieldParam <&>
    \addParam ->
    do
        paramTag <- DataOps.genNewTag
        addParam Nothing mkNewArg (BinderKindLet letLam) storedLam
            ((fieldTags ++) . pure) paramTag
        convertVarToGetFieldParam var paramTag (storedLam ^. Params.slLam)
        storedLam ^. Params.slLambdaProp & pure
    where
        mkNewArg = V.LVar var & V.BLeaf & ExprIRef.newValI

addLetParam ::
    Monad m =>
    V.Var -> Redex (Input.Payload m a) -> ConvertM m (T m (ValP m))
addLetParam var redex =
    case storedRedex ^. Redex.arg . val of
    V.BLam lam | isVarAlwaysApplied (redex ^. Redex.lam) ->
        case redex ^. Redex.arg . ann . Input.inferredType of
        Pure (T.TFun (FuncType (Pure (T.TRecord composite)) _))
            | FlatRowExtends fieldsMap (Pure T.REmpty) <- composite ^. T.flatRow
            , let fields = Map.toList fieldsMap
            , Params.isParamAlwaysUsedWithGetField lam ->
            addFieldToLetParamsRecord
                (fields <&> fst) var (storedRedex ^. Redex.lam) storedLam
        _ -> convertLetParamToRecord var (storedRedex ^. Redex.lam) storedLam
        where
            storedLam = Params.StoredLam lam (storedRedex ^. Redex.arg . ann)
    _ -> convertLetToLam var storedRedex & pure
    where
        storedRedex = redex <&> (^. Input.stored)

sameLet :: Redex (ValP m) -> ValP m
sameLet redex = redex ^. Redex.arg . ann

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

processLet :: Monad m => Redex (Input.Payload m a) -> ConvertM m (T m (ValP m))
processLet redex =
    do
        scopeInfo <- Lens.view ConvertM.scScopeInfo
        let usedLocalVars =
                redex ^.. Redex.arg . ExprLens.valLeafs . V._LVar
                & ordNub
                & filter (`Set.member` innerScopeLocalVars)
        let varsExitingScope =
                case scopeInfo ^. ConvertM.siMOuter of
                Nothing -> usedLocalVars
                Just outerScopeInfo ->
                    filter (`Map.notMember` outerScope) usedLocalVars
                    where
                        outerScope = outerScopeInfo ^. ConvertM.osiScope . V.scopeVarTypes
        case varsExitingScope of
            [] -> sameLet (redex <&> (^. Input.stored)) & pure & pure
            [x] -> addLetParam x redex
            _ -> error "multiple osiVarsUnderPos not expected!?"
    where
        innerScopeLocalVars =
            redex ^. Redex.arg . ann . Input.localsInScope & Set.fromList

makeFloatLetToOuterScope ::
    Monad m =>
    (ValI m -> T m ()) ->
    Redex (Input.Payload m a) ->
    ConvertM m (T m ExtractDestination)
makeFloatLetToOuterScope setTopLevel redex =
    (,,,)
    <$> processLet newRedex
    <*> Lens.view id
    <*> moveToGlobalScope
    <*> ConvertM.postProcessWith
    <&>
    \(makeNewLet, ctx, floatToGlobal, postProcess) ->
    do
        redex ^. Redex.lam . V.lamOut . ann . Input.stored .
            Property.pVal & setTopLevel
        newLetP <- makeNewLet
        r <-
            case ctx ^. ConvertM.scScopeInfo . ConvertM.siMOuter of
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
                        & Def.pruneDeps (redex ^. Redex.arg)
            Just outerScopeInfo ->
                EntityId.ofValI (redex ^. Redex.arg . ann . Input.stored . Property.pVal) <$
                DataOps.redexWrapWithGivenParam param
                (Property.value newLetP) (outerScopeInfo ^. ConvertM.osiPos)
                <&> ExtractToLet
        r <$ postProcess fixUsages
    where
        param = redex ^. Redex.lam . V.lamIn
        fixUsages _ =
            Load.readValAndAddProperties (newRedex ^. Redex.lam . V.lamOut . ann . Input.stored)
            >>= SubExprs.onGetVars (void . DataOps.applyHoleTo) (redex ^. Redex.lam . V.lamIn)
        newRedex =
            redex
            & Redex.lam . V.lamOut . ann . Input.stored . Property.pSet .~ setTopLevel
