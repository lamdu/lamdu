-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, TupleSections, TypeApplications, TypeOperators #-}
module Lamdu.Sugar.Convert.Load
    ( InferOut(..), irVal, irCtx
    , inferDef
    , inferDefExpr
    , makeNominalsMap
    , readValAndAddProperties
    , InferFunc, unmemoizedInfer
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer (InferResult(..), _InferResult, infer)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalDecl, nScheme)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Unify (unify)
import           Hyper.Unify.Apply (applyBindings)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import           Lamdu.Calc.Infer (PureInfer, runPureInfer, InferState(..), loadDeps, emptyPureInferState, varGen)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Sugar.Convert.Input (EvalResultsForExpr(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type InferFunc a =
    Definition.Expr (Tree (Ann a) V.Term) ->
    PureInfer (Tree V.Scope UVar) (Tree (Ann (a :*: InferResult UVar)) V.Term, Tree V.Scope UVar)

unmemoizedInfer :: InferFunc a
unmemoizedInfer defExpr =
    do
        addDeps <- loadDeps (defExpr ^. Definition.exprFrozenDeps)
        scope <- Lens.view id <&> addDeps
        infer (defExpr ^. Definition.expr) & Reader.local (const scope)
            <&> (, scope)

preparePayloads ::
    InferState ->
    Tree V.Scope UVar ->
    Map NominalId (Tree Pure (NominalDecl T.Type)) ->
    CurAndPrev (EvalResults (ValI m)) ->
    Annotated (Tree (HRef m) V.Term, Tree Pure T.Type, Tree UVar T.Type) V.Term ->
    Annotated (Input.Payload m ()) V.Term
preparePayloads inferState topLevelScope nomsMap evalRes inferredVal =
    inferredVal
    & hflipped . hmapped1 . Lens._Wrapped %~ f
    & Input.preparePayloads
    & Input.initScopes inferState topLevelScope []
    where
        f (valIProp, typ, inferRes) =
            ( eId
            , \varRefs ->
              Input.Payload
              { Input._varRefsOfLambda = varRefs
              , Input._entityId = eId
              , Input._localsInScope = []
              , Input._stored = valIProp
              , Input._inferredType = typ
              , Input._inferResult = inferRes
              , Input._inferScope = V.emptyScope -- UGLY: This is initialized by initScopes
              , Input._evalResults = evalRes <&> exprEvalRes nomsMap typ (valIProp ^. ExprIRef.iref)
              , Input._userData = ()
              }
            )
            where
                eId = valIProp ^. ExprIRef.iref & EntityId.ofValI

exprEvalRes ::
    Map NominalId (Tree Pure (NominalDecl T.Type)) ->
    Tree Pure T.Type ->
    ValI m ->
    EvalResults (ValI m) ->
    EvalResultsForExpr
exprEvalRes nomsMap typ pl r =
    EvalResultsForExpr
    { _eResults = r ^. erExprValues . Lens.ix pl <&> addTypes nomsMap typ
    , _eAppliesOfLam =
        case typ of
        Pure (T.TFun (FuncType paramType _)) ->
            r ^. erAppliesOfLam . Lens.ix pl
            <&> Lens.mapped . Lens.mapped %~ addTypes nomsMap paramType
        _ | r ^. erAppliesOfLam & Map.null -> Map.empty
            | otherwise -> error "Have non-empty erAppliesOfLam for non-function-type"
    }

makeNominalsMap ::
    Monad m =>
    [NominalId] -> T m (Map NominalId (Tree Pure (NominalDecl T.Type)))
makeNominalsMap tids =
    traverse_ loadForTid tids
    & (`State.execStateT` mempty)
    <&> Map.mapMaybe id
    where
        loadForTid tid =
            do
                loaded <- State.get
                unless (Map.member tid loaded) $
                    do
                        nom <- ExprLoad.nominal tid & lift
                        Map.insert tid nom loaded & State.put
                        nom ^.. Lens._Just . _Pure . nScheme . sTyp . ExprLens.tIds
                            & traverse_ loadForTid

readValAndAddProperties ::
    Monad m => Tree (HRef m) V.Term -> T m (Tree (Ann (HRef m)) V.Term)
readValAndAddProperties prop =
    ExprIRef.readRecursively (prop ^. ExprIRef.iref)
    <&> hflipped . hmapped1 %~ (:*: Const ())
    <&> ExprIRef.toHRefs (prop ^. ExprIRef.setIref)
    <&> hflipped . hmapped1 %~ (^. Lens._1)

data InferOut m = InferOut
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irCtx :: InferState
    }
Lens.makeLenses ''InferOut

resolve ::
    RTraversable t =>
    Annotated (Tree (HRef m) V.Term, Tree UVar T.Type) t ->
    PureInfer a (Annotated (Tree (HRef m) V.Term, Tree Pure T.Type, Tree UVar T.Type) t)
resolve =
    hflipped (htraverse (const (Lens._Wrapped f)))
    where
        f ::
            (Tree (HRef m) V.Term, Tree UVar T.Type) ->
            PureInfer a (Tree (HRef m) V.Term, Tree Pure T.Type, Tree UVar T.Type)
        f (stored, inferred) =
            applyBindings inferred
            <&> \x -> (stored, x, inferred)

runInferResult ::
    Monad m =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    PureInfer (Tree V.Scope UVar) (Annotated (Tree (HRef m) V.Term, Tree UVar T.Type) V.Term, Tree V.Scope UVar) ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
runInferResult _monitors evalRes act =
    -- TODO: use _monitors
    case runPureInfer V.emptyScope (InferState emptyPureInferState varGen) act of
    Left x -> Left x & pure
    Right ((inferredTerm, topLevelScope), inferState0) ->
        ParamList.loadForLambdas inferredTerm
        >>=
        \doLoad ->
        case runPureInfer V.emptyScope inferState0 doLoad of
        Left x -> Left x & pure
        Right (_, inferState1) ->
            case resolve inferredTerm & runPureInfer V.emptyScope inferState1 of
            Left x -> Left x & pure
            Right (resolvedTerm, _inferState2) ->
                hfoldMap (const (^.. Lens._Wrapped . _2 . ExprLens.tIds)) (_HFlip # resolvedTerm)
                & makeNominalsMap
                <&>
                \nomsMap ->
                InferOut
                ( preparePayloads inferState1 topLevelScope nomsMap evalRes resolvedTerm
                    & hflipped %~ hmap (const (Lens._Wrapped %~ setUserData))
                ) inferState1
                & Right
    where
        setUserData pl = pl & Input.userData %~ \() -> [pl ^. Input.entityId]

inferDef ::
    Monad m =>
    InferFunc (HRef m) -> Debug.Monitors ->
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Tree (Ann (HRef m)) V.Term) -> V.Var ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
inferDef inferFunc monitors results defExpr defVar =
    do
        defTv <- newUnbound
        (inferredVal, scope) <-
            inferFunc defExpr
            & Reader.local (V.scopeVarTypes . Lens.at defVar ?~ MkHFlip (GMono defTv))
        (inferredVal, scope) <$ unify defTv (inferredVal ^. hAnn . Lens._2 . _InferResult . _ANode)
    <&> Lens._1 . hflipped . hmapped1 %~ (\(x :*: InferResult r) -> Const (x, r ^. _ANode))
    & runInferResult monitors results

inferDefExpr ::
    Monad m =>
    InferFunc (HRef m) -> Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Tree (Ann (HRef m)) V.Term) ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
inferDefExpr inferFunc monitors results defExpr =
    inferFunc defExpr
    <&> Lens._1 . hflipped . hmapped1 %~ (\(x :*: InferResult r) -> Const (x, r ^. _ANode))
    & runInferResult monitors results
