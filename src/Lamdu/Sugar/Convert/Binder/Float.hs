{-# LANGUAGE PatternGuards, NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Vars as TV
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
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
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

moveToGlobalScope ::
    Monad m => ConvertM.Context m -> V.Var -> Definition.Expr (ValI m) -> T m ()
moveToGlobalScope ctx param defExpr =
    do
        inferRes <-
            Definition.expr ExprIRef.readVal defExpr
            >>= (`Load.inferCheckDef` param)
        scheme <-
            case inferRes of
            Left err -> fail ("extract to global scope failed inference: " ++ show (prettyShow err))
            Right (inferredVal, inferContext) ->
                inferredVal ^. Val.payload . _1 . Infer.plType
                & Infer.makeScheme inferContext
                & return
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
        return ()

data NewLet m = NewLet
    { nlIRef :: ValI m
    , nlMVarToTags :: Maybe VarToTags
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
convertLetToLam varToReplace redex =
    do
        (ParamAddResultNewVar _ newParam, newValI) <-
            Params.convertBinderToFunction mkArg
            (BinderKindLet (redex ^. Redex.lam)) (redex ^. Redex.arg)
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValBody (Property.value prop)
        SubExprs.onGetVars toNewParam varToReplace (redex ^. Redex.arg)
        return NewLet
            { nlIRef = newValI
            , nlMVarToTags = Nothing
            }
    where
        mkArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

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
    V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m -> T m (NewLet m)
convertLetParamToRecord varToReplace letLam storedLam =
    do
        vtt <-
            Params.convertToRecordParams
            mkNewArg (BinderKindLet letLam) storedLam Params.NewParamAfter
        convertVarToGetFieldParam varToReplace (vttNewTag vtt ^. tagVal)
            (storedLam ^. Params.slLam)
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlMVarToTags = Just vtt
            }
    where
        mkNewArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

addFieldToLetParamsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m ->
    T m (NewLet m)
addFieldToLetParamsRecord fieldTags varToReplace letLam storedLam =
    do
        newParamTag <-
            Params.addFieldParam Nothing mkNewArg (BinderKindLet letLam) storedLam
            ((fieldTags ++) . pure)
        convertVarToGetFieldParam varToReplace (newParamTag ^. tagVal)
            (storedLam ^. Params.slLam)
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlMVarToTags = Nothing
            }
    where
        mkNewArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

addLetParam ::
    Monad m => V.Var -> Redex (Input.Payload m a) -> T m (NewLet m)
addLetParam varToReplace redex =
    case storedRedex ^. Redex.arg . Val.body of
    V.BLam lam | isVarAlwaysApplied (redex ^. Redex.lam) ->
        case redex ^. Redex.arg . Val.payload . Input.inferred . Infer.plType of
        T.TFun (T.TRecord composite) _
            | Just fields <- composite ^? orderedClosedFlatComposite
            , Params.isParamAlwaysUsedWithGetField lam ->
            addFieldToLetParamsRecord
                (fields <&> fst) varToReplace (storedRedex ^. Redex.lam) storedLam
        _ -> convertLetParamToRecord varToReplace (storedRedex ^. Redex.lam) storedLam
        where
            storedLam = Params.StoredLam lam (storedRedex ^. Redex.arg . Val.payload)
    _ -> convertLetToLam varToReplace storedRedex
    where
        storedRedex = redex <&> (^. Input.stored)

sameLet :: Redex (ValIProperty m) -> NewLet m
sameLet redex =
    NewLet
    { nlIRef = redex ^. Redex.arg . Val.payload & Property.value
    , nlMVarToTags = Nothing
    }

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

processLet ::
    Monad m => ConvertM.ScopeInfo f -> Redex (Input.Payload m a) -> T m (NewLet m)
processLet scopeInfo redex =
    case varsExitingScope of
    [] -> sameLet (redex <&> (^. Input.stored)) & return
    [x] -> addLetParam x redex
    _ -> error "multiple osiVarsUnderPos not expected!?"
    <* maybeDetach
    where
        mRecursiveRef = scopeInfo ^. ConvertM.siRecursiveRef
        mDefI = mRecursiveRef ^? Lens._Just . ConvertM.rrDefI <&> ExprIRef.globalId
        isRecursiveDefRef var = mDefI == Just var
        maybeDetach
            | TV.null skolemsExitingScope = return ()
            | otherwise =
                Load.readValAndAddProperties (redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored)
                >>= SubExprs.onGetVars (void . DataOps.applyHoleTo) (redex ^. Redex.lam . V.lamParamId)
        innerScope =
            redex ^. Redex.arg . Val.payload . Input.inferred . Infer.plScope
        usedLocalVars =
            redex ^.. Redex.arg . ExprLens.valLeafs . V._LVar
            & ordNub
            & filter (not . isRecursiveDefRef)
            & filter (`Map.member` Infer.scopeToTypeMap innerScope)
        innerSkolems = Infer.skolems innerScope ^. Infer.skolemScopeVars
        (varsExitingScope, skolemsExitingScope) =
            case scopeInfo ^. ConvertM.siMOuter of
            Nothing -> (usedLocalVars, mempty)
            Just outerScopeInfo ->
                ( filter (`Map.notMember` Infer.scopeToTypeMap outerScope) usedLocalVars
                , innerSkolems `TV.difference` outerSkolems
                )
                where
                    outerSkolems = Infer.skolems outerScope ^. Infer.skolemScopeVars
                    outerScope = outerScopeInfo ^. ConvertM.osiScope

floatLetToOuterScope ::
    Monad m =>
    (ValI m -> T m ()) ->
    Redex (Input.Payload m a) -> ConvertM.Context m ->
    T m ExtractFloatResult
floatLetToOuterScope setTopLevel redex ctx =
    do
        redex ^. Redex.lam . V.lamResult . Val.payload . Input.stored . Property.pVal & setTopLevel
        newLet <-
            redex
            & Redex.lam . V.lamResult . Val.payload . Input.stored . Property.pSet .~ setTopLevel
            & processLet (ctx ^. ConvertM.scScopeInfo)
        resultEntity <-
            case ctx ^. ConvertM.scScopeInfo . ConvertM.siMOuter of
            Nothing ->
                EntityId.ofIRef (ExprIRef.defI param) <$
                moveToGlobalScope ctx param innerDefExpr
                <&> ExtractToDef
                where
                    innerDefExpr = Definition.Expr (nlIRef newLet) innerDeps
                    innerDeps =
                        -- Outer deps, pruned:
                        ctx ^. ConvertM.scFrozenDeps . Property.pVal
                        & addRecursiveRefAsDep
                        & Definition.Expr (redex ^. Redex.arg)
                        & Definition.pruneDefExprDeps
            Just outerScopeInfo ->
                EntityId.ofLambdaParam param <$
                DataOps.redexWrapWithGivenParam param (nlIRef newLet) (outerScopeInfo ^. ConvertM.osiPos)
                <&> ExtractToLet
        return ExtractFloatResult
            { efrNewEntity = resultEntity
            , efrMVarToTags = nlMVarToTags newLet
            }
    where
        addRecursiveRefAsDep =
            case ctx ^. ConvertM.scScopeInfo . ConvertM.siRecursiveRef of
            Nothing -> id
            Just (ConvertM.RecursiveRef defI defType) ->
                Infer.depsGlobalTypes . Lens.at (ExprIRef.globalId defI) ?~ defType
        param = redex ^. Redex.lam . V.lamParamId

makeFloatLetToOuterScope ::
    Monad m =>
    (ValI m -> T m ()) -> Redex (Input.Payload m a) ->
    ConvertM m (T m ExtractFloatResult)
makeFloatLetToOuterScope setTopLevel redex =
    ConvertM.readContext <&> floatLetToOuterScope setTopLevel redex
