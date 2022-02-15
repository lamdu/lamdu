-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Lamdu.Sugar.Convert.Load
    ( InferOut(..), irVal, irCtx
    , inferDef
    , makeNominalsMap
    , readValAndAddProperties
    , InferFunc, unmemoizedInfer
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer
import           Hyper.Syntax.Nominal (NominalDecl, nScheme)
import           Hyper.Syntax.Scheme (sTyp)
import           Hyper.Type.Functor (_F)
import           Hyper.Unify (UVar, unify)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.New (newUnbound)
import           Lamdu.Calc.Infer (PureInfer, runPureInfer, InferState(..), loadDeps, emptyPureInferState, varGen)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
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
        infer (defExpr ^. Definition.expr) & local (const scope)
            <&> (, scope)

preparePayloads ::
    V.Scope # UVar ->
    Ann (HRef m :*: InferResult (Pure :*: UVar)) # V.Term ->
    Ann (Input.Payload m) # V.Term
preparePayloads topLevelScope inferredVal =
    inferredVal
    & hflipped %~ hmap (const f)
    & Input.preprocess topLevelScope []
    where
        f (valIProp :*: inferRes) =
            Input.Payload
            { Input._varRefsOfLambda = [] -- TODO
            , Input._entityId = eId
            , Input._localsInScope = []
            , Input._stored = valIProp
            , Input._inferRes = inferRes
            , Input._inferScope = V.emptyScope -- UGLY: This is initialized by SugarInput.prep
            }
            where
                eId = valIProp ^. ExprIRef.iref . _F & IRef.uuid & EntityId.EntityId

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
                        Map.insert tid (nom ^? Lens._Right) loaded & State.put
                        nom ^.. Lens._Right . _Pure . nScheme . sTyp . ExprLens.tIds
                            & traverse_ loadForTid

readValAndAddProperties ::
    Monad m => HRef m # V.Term -> T m (Ann (HRef m) # V.Term)
readValAndAddProperties prop =
    ExprIRef.readRecursively (prop ^. ExprIRef.iref)
    <&> hflipped %~ hmap (const (:*: Const ()))
    <&> ExprIRef.toHRefs (prop ^. ExprIRef.setIref)
    <&> hflipped %~ hmap (const (^. _1))

data InferOut m = InferOut
    { _irVal :: Ann (Input.Payload m) # V.Term
    , _irCtx :: InferState
    }
Lens.makeLenses ''InferOut

runInferResult ::
    Debug.Monitors ->
    PureInfer (V.Scope # UVar) (Ann (HRef m :*: InferResult UVar) # V.Term, V.Scope # UVar) ->
    Either (Pure # T.TypeError) (InferOut m)
runInferResult _monitors act =
    -- TODO: use _monitors
    case runPureInfer V.emptyScope (InferState emptyPureInferState varGen) act of
    Left x -> Left x
    Right ((inferredTerm, topLevelScope), inferState0) ->
        case inferUVarsApplyBindings inferredTerm & runPureInfer () inferState0 of
        Left x -> Left x
        Right (resolvedTerm, _inferState1) ->
            InferOut
            ( preparePayloads topLevelScope resolvedTerm
            ) inferState0
            & Right

inferDef ::
    InferFunc (HRef m) -> Debug.Monitors ->
    Definition.Expr (Ann (HRef m) # V.Term) -> V.Var ->
    Either (Pure # T.TypeError) (InferOut m)
inferDef inferFunc monitors defExpr defVar =
    do
        defTv <- newUnbound
        (inferredVal, scope) <-
            inferFunc defExpr
            & local (V.scopeVarTypes . Lens.at defVar ?~ MkHFlip (GMono defTv))
        (inferredVal, scope) <$ unify defTv (inferredVal ^. hAnn . _2 . inferResult)
    & runInferResult monitors
