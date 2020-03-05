-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, TupleSections, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
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
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer
import           Hyper.Recurse
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalDecl, nScheme)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune)
import           Hyper.Unify (unify)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import           Lamdu.Calc.Infer (PureInfer, runPureInfer, InferState(..), loadDeps, emptyPureInferState, varGen)
import qualified Lamdu.Calc.Lens as ExprLens
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
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type InferFunc a =
    Definition.Expr (Ann a # V.Term) ->
    PureInfer (V.Scope # UVar) (Ann (a :*: InferResult UVar) # V.Term, V.Scope # UVar)

unmemoizedInfer :: InferFunc a
unmemoizedInfer defExpr =
    do
        addDeps <- loadDeps (defExpr ^. Definition.exprFrozenDeps)
        scope <- Lens.view id <&> addDeps
        infer (defExpr ^. Definition.expr) & Reader.local (const scope)
            <&> (, scope)

class LookupEvalRes h where
    lookupEvalRes ::
        CurAndPrev EvalResults ->
        Map NominalId (Pure # NominalDecl T.Type) ->
        (HRef m :*: InferResult (Pure :*: UVar)) # h ->
        CurAndPrev EvalResultsForExpr
    lookupEvalRes _ _ _ = CurAndPrev Input.emptyEvalResults Input.emptyEvalResults
    lookupEvalResRecursive :: Proxy h -> Dict (HNodesConstraint h LookupEvalRes)

instance LookupEvalRes V.Term where
    lookupEvalRes evalRes nomsMap (valIProp :*: inferRes) =
        evalRes
        <&> exprEvalRes nomsMap (inferRes ^. inferResult . Lens._1)
            (valIProp ^. ExprIRef.iref)
    lookupEvalResRecursive _ = Dict

instance LookupEvalRes (HCompose Prune T.Type) where
    lookupEvalResRecursive _ = Dict

instance LookupEvalRes (HCompose Prune T.Row) where
    lookupEvalResRecursive _ = Dict

instance Recursive LookupEvalRes where
    recurse = lookupEvalResRecursive . proxyArgument

preparePayloads ::
    V.Scope # UVar ->
    Map NominalId (Pure # NominalDecl T.Type) ->
    CurAndPrev EvalResults ->
    Ann (HRef m :*: InferResult (Pure :*: UVar)) # V.Term ->
    Ann (Input.Payload m ()) # V.Term
preparePayloads topLevelScope nomsMap evalRes inferredVal =
    inferredVal
    & hflipped %~ hmap (Proxy @LookupEvalRes #> f)
    & Input.preparePayloads
    & Input.initScopes topLevelScope []
    where
        f pl@(valIProp :*: inferRes) =
            Input.PreparePayloadInput
            { Input.ppEntityId = eId
            , Input.ppMakePl =
                \varRefs ->
                Input.Payload
                { Input._varRefsOfLambda = varRefs
                , Input._entityId = eId
                , Input._localsInScope = []
                , Input._stored = valIProp
                , Input._inferRes = inferRes
                , Input._inferScope = V.emptyScope -- UGLY: This is initialized by initScopes
                , Input._evalResults = lookupEvalRes evalRes nomsMap pl
                , Input._userData = ()
                }
            }
            where
                eId = valIProp ^. ExprIRef.iref . _F & IRef.uuid & EntityId.EntityId

exprEvalRes ::
    Map NominalId (Pure # NominalDecl T.Type) ->
    Pure # T.Type ->
    ValI m ->
    EvalResults ->
    EvalResultsForExpr
exprEvalRes nomsMap typ pl r =
    EvalResultsForExpr
    { _eResults = r ^. erExprValues . Lens.ix uuid <&> addTypes nomsMap typ
    , _eAppliesOfLam =
        case typ of
        Pure (T.TFun (FuncType paramType _)) ->
            r ^. erAppliesOfLam . Lens.ix uuid
            <&> Lens.mapped . Lens.mapped %~ addTypes nomsMap paramType
        _ | r ^. erAppliesOfLam . Lens.ix uuid & Map.null -> Map.empty
            | otherwise -> error "Have non-empty erAppliesOfLam for non-function-type"
    }
    where
        uuid = IRef.uuid (pl ^. _F)

makeNominalsMap ::
    Monad m =>
    [NominalId] -> T m (Map NominalId (Pure # NominalDecl T.Type))
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
    Monad m => HRef m # V.Term -> T m (Ann (HRef m) # V.Term)
readValAndAddProperties prop =
    ExprIRef.readRecursively (prop ^. ExprIRef.iref)
    <&> hflipped %~ hmap (const (:*: Const ()))
    <&> ExprIRef.toHRefs (prop ^. ExprIRef.setIref)
    <&> hflipped %~ hmap (const (^. Lens._1))

data InferOut m = InferOut
    { _irVal :: Ann (Input.Payload m [EntityId]) # V.Term
    , _irCtx :: InferState
    }
Lens.makeLenses ''InferOut

class InferResTids h where
    inferResTids :: InferResult (Pure :*: UVar) # h -> [T.NominalId]
    inferResTidsRecursive :: Proxy h -> Dict (HNodesConstraint h InferResTids)

instance InferResTids V.Term where
    inferResTids = (^.. inferResult . _1 . ExprLens.tIds)
    inferResTidsRecursive _ = Dict

instance InferResTids (HCompose Prune T.Type) where
    inferResTids _ = []
    inferResTidsRecursive _ = Dict

instance InferResTids (HCompose Prune T.Row) where
    inferResTids _ = []
    inferResTidsRecursive _ = Dict

instance Recursive InferResTids where
    recurse = inferResTidsRecursive . proxyArgument

runInferResult ::
    Monad m =>
    Debug.Monitors -> CurAndPrev EvalResults ->
    PureInfer (V.Scope # UVar) (Ann (HRef m :*: InferResult UVar) # V.Term, V.Scope # UVar) ->
    T m (Either (Pure # T.TypeError) (InferOut m))
runInferResult _monitors evalRes act =
    -- TODO: use _monitors
    case runPureInfer V.emptyScope (InferState emptyPureInferState varGen) act of
    Left x -> Left x & pure
    Right ((inferredTerm, topLevelScope), inferState0) ->
        case inferUVarsApplyBindings inferredTerm & runPureInfer () inferState0 of
        Left x -> Left x & pure
        Right (resolvedTerm, _inferState1) ->
            hfoldMap (Proxy @InferResTids #> inferResTids . (^. _2))
            (_HFlip # resolvedTerm)
            & makeNominalsMap
            <&>
            \nomsMap ->
            InferOut
            ( preparePayloads topLevelScope nomsMap evalRes resolvedTerm
                & hflipped %~ hmap (const setUserData)
            ) inferState0
            & Right
    where
        setUserData pl = pl & Input.userData %~ \() -> [pl ^. Input.entityId]

inferDef ::
    Monad m =>
    InferFunc (HRef m) -> Debug.Monitors ->
    CurAndPrev EvalResults ->
    Definition.Expr (Ann (HRef m) # V.Term) -> V.Var ->
    T m (Either (Pure # T.TypeError) (InferOut m))
inferDef inferFunc monitors results defExpr defVar =
    do
        defTv <- newUnbound
        (inferredVal, scope) <-
            inferFunc defExpr
            & Reader.local (V.scopeVarTypes . Lens.at defVar ?~ MkHFlip (GMono defTv))
        (inferredVal, scope) <$ unify defTv (inferredVal ^. hAnn . Lens._2 . inferResult)
    & runInferResult monitors results

inferDefExpr ::
    Monad m =>
    InferFunc (HRef m) -> Debug.Monitors -> CurAndPrev EvalResults ->
    Definition.Expr (Ann (HRef m) # V.Term) ->
    T m (Either (Pure # T.TypeError) (InferOut m))
inferDefExpr inferFunc monitors results defExpr =
    inferFunc defExpr
    & runInferResult monitors results
