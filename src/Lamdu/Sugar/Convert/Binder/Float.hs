{-# LANGUAGE PatternGuards #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Vars as TV
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder.Params as Params
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderedClosedFlatComposite)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

moveToGlobalScope ::
    Monad m => ConvertM.Context m -> V.Var -> Definition.Expr (ValI m) -> T m ()
moveToGlobalScope ctx param defExpr =
    do
        inferRes <-
            Definition.expr ExprIRef.readVal defExpr
            >>= (Load.inferCheckDef (ctx ^. ConvertM.scDebugMonitors) ?? param)
        scheme <-
            case inferRes of
            Left err -> fail ("extract to global scope failed inference: " ++ show (prettyShow err))
            Right (inferredVal, inferContext) ->
                inferredVal ^. Val.payload . _1 . Infer.plType
                & Infer.makeScheme inferContext
                & pure
        DataOps.newPublicDefinitionToIRef
            (ctx ^. ConvertM.scCodeAnchors)
            (Definition.Definition (Definition.BodyExpr defExpr) scheme ())
            (ExprIRef.defI param)
        Infer.depsGlobalTypes . Lens.at param ?~ scheme
            & Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
        -- Prune outer def's deps (some used only in inner) and update
        -- our type which may become generalized due to
        -- extraction/generalization of the inner type
        PostProcess.GoodExpr <- ctx ^. ConvertM.scPostProcessRoot
        pure ()

-- TODO: Remove this
newtype NewLet m = NewLet
    { nlIRef :: ValI m
    }

isVarAlwaysApplied :: V.Lam (Val a) -> Bool
isVarAlwaysApplied (V.Lam var body) =
    go False body
    where
        go isApplied (Val _ (V.BLeaf (V.LVar v))) | v == var = isApplied
        go _ (Val _ (V.BApp (V.Apply f a))) = go True f && go False a
        go _ v = all (go False) (v ^.. Val.body . Lens.traverse)

convertLetToLam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> T m (NewLet m)
convertLetToLam var redex =
    do
        (newParam, newValI) <-
            Params.convertBinderToFunction mkArg
            (BinderKindLet (redex ^. Redex.lam)) (redex ^. Redex.arg)
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValBody (Property.value prop)
        SubExprs.onGetVars toNewParam var (redex ^. Redex.arg)
        NewLet newValI & pure
    where
        mkArg = V.LVar var & V.BLeaf & ExprIRef.newValBody

convertVarToGetFieldParam ::
    Monad m =>
    V.Var -> T.Tag -> V.Lam (Val (ValIProperty m)) -> T m ()
convertVarToGetFieldParam oldVar paramTag (V.Lam lamVar lamBody) =
    SubExprs.onGetVars toNewParam oldVar lamBody
    where
        toNewParam prop =
            V.LVar lamVar & V.BLeaf
            & ExprIRef.newValBody
            <&> (`V.GetField` paramTag) <&> V.BGetField
            >>= ExprIRef.writeValBody (Property.value prop)

convertLetParamToRecord ::
    Monad m =>
    V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m ->
    ConvertM m (T m (NewLet m))
convertLetParamToRecord var letLam storedLam =
    Params.convertToRecordParams <&> \toRecordParams ->
    do
        tagForVar <- Anchors.assocTag var & Property.getP
        DataOps.genNewTag
            >>= toRecordParams mkNewArg (BinderKindLet letLam)
                storedLam Params.NewParamAfter
        convertVarToGetFieldParam var tagForVar (storedLam ^. Params.slLam)
        storedLam ^. Params.slLambdaProp . Property.pVal & NewLet & pure
    where
        mkNewArg = V.LVar var & V.BLeaf & ExprIRef.newValBody

addFieldToLetParamsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m ->
    ConvertM m (T m (NewLet m))
addFieldToLetParamsRecord fieldTags var letLam storedLam =
    Params.addFieldParam <&>
    \addParam ->
    do
        paramTag <- DataOps.genNewTag
        addParam Nothing mkNewArg (BinderKindLet letLam) storedLam
            ((fieldTags ++) . pure) paramTag
        convertVarToGetFieldParam var paramTag (storedLam ^. Params.slLam)
        storedLam ^. Params.slLambdaProp . Property.pVal & NewLet & pure
    where
        mkNewArg = V.LVar var & V.BLeaf & ExprIRef.newValBody

addLetParam ::
    Monad m =>
    V.Var -> Redex (Input.Payload m a) -> ConvertM m (T m (NewLet m))
addLetParam var redex =
    case storedRedex ^. Redex.arg . Val.body of
    V.BLam lam | isVarAlwaysApplied (redex ^. Redex.lam) ->
        case redex ^. Redex.arg . Val.payload . Input.inferred . Infer.plType of
        T.TFun (T.TRecord composite) _
            | Just fields <- composite ^? orderedClosedFlatComposite
            , Params.isParamAlwaysUsedWithGetField lam ->
            addFieldToLetParamsRecord
                (fields <&> fst) var (storedRedex ^. Redex.lam) storedLam
        _ -> convertLetParamToRecord var (storedRedex ^. Redex.lam) storedLam
        where
            storedLam = Params.StoredLam lam (storedRedex ^. Redex.arg . Val.payload)
    _ -> convertLetToLam var storedRedex & pure
    where
        storedRedex = redex <&> (^. Input.stored)

sameLet :: Redex (ValIProperty m) -> NewLet m
sameLet redex = redex ^. Redex.arg . Val.payload & Property.value & NewLet

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

processLet :: Monad m => Redex (Input.Payload m a) -> ConvertM m (T m (NewLet m))
processLet redex =
    do
        scopeInfo <- Lens.view ConvertM.scScopeInfo
        let mRecursiveRef = scopeInfo ^. ConvertM.siRecursiveRef
        let mDefI = mRecursiveRef ^? Lens._Just . ConvertM.rrDefI <&> ExprIRef.globalId
        let isRecursiveDefRef var = mDefI == Just var
        let usedLocalVars =
                redex ^.. Redex.arg . ExprLens.valLeafs . V._LVar
                & ordNub
                & filter (not . isRecursiveDefRef)
                & filter (`Map.member` Infer.scopeToTypeMap innerScope)
        let (varsExitingScope, skolemsExitingScope) =
                case scopeInfo ^. ConvertM.siMOuter of
                Nothing -> (usedLocalVars, mempty)
                Just outerScopeInfo ->
                    ( filter (`Map.notMember` Infer.scopeToTypeMap outerScope) usedLocalVars
                    , innerSkolems `TV.difference` outerSkolems
                    )
                    where
                        outerSkolems = Infer.skolems outerScope ^. Infer.skolemScopeVars
                        outerScope = outerScopeInfo ^. ConvertM.osiScope
        let maybeDetach
                | TV.null skolemsExitingScope = pure ()
                | otherwise =
                    Load.readValAndAddProperties (redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored)
                    >>= SubExprs.onGetVars (void . DataOps.applyHoleTo) (redex ^. Redex.lam . V.lamParamId)
        case varsExitingScope of
            [] -> sameLet (redex <&> (^. Input.stored)) & pure & pure
            [x] -> addLetParam x redex
            _ -> error "multiple osiVarsUnderPos not expected!?"
            <&> (<* maybeDetach)
    where
        innerScope =
            redex ^. Redex.arg . Val.payload . Input.inferred . Infer.plScope
        innerSkolems = Infer.skolems innerScope ^. Infer.skolemScopeVars

makeFloatLetToOuterScope ::
    Monad m =>
    (ValI m -> T m ()) ->
    Redex (Input.Payload m a) ->
    ConvertM m (T m ExtractDestination)
makeFloatLetToOuterScope setTopLevel redex =
    (,)
    <$>
    ( redex
    & Redex.lam . V.lamResult . Val.payload . Input.stored . Property.pSet .~
        setTopLevel
    & processLet
    )
    <*> Lens.view id
    <&>
    \(makeNewLet, ctx) ->
    do
        redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored .
            Property.pVal & setTopLevel
        newLet <- makeNewLet
        case ctx ^. ConvertM.scScopeInfo . ConvertM.siMOuter of
            Nothing ->
                EntityId.ofIRef (ExprIRef.defI param) <$
                moveToGlobalScope ctx param innerDefExpr
                <&> ExtractToDef
                where
                    addRecursiveRefAsDep =
                        case ctx ^. ConvertM.scScopeInfo . ConvertM.siRecursiveRef of
                        Nothing -> id
                        Just (ConvertM.RecursiveRef defI defType) ->
                            Infer.depsGlobalTypes . Lens.at (ExprIRef.globalId defI) ?~ defType
                    innerDefExpr = Definition.Expr (nlIRef newLet) innerDeps
                    innerDeps =
                        -- Outer deps, pruned:
                        ctx ^. ConvertM.scFrozenDeps . Property.pVal
                        & addRecursiveRefAsDep
                        & Definition.Expr (redex ^. Redex.arg)
                        & Definition.pruneDefExprDeps
            Just outerScopeInfo ->
                EntityId.ofBinder param <$
                DataOps.redexWrapWithGivenParam param (nlIRef newLet) (outerScopeInfo ^. ConvertM.osiPos)
                <&> ExtractToLet
    where
        param = redex ^. Redex.lam . V.lamParamId
