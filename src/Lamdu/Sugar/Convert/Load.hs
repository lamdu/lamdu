-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, TupleSections, TypeApplications #-}
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
import           Data.Property (Property)
import qualified Data.Property as Property
import           Hyper (Tree, Pure(..), _Pure, annotations, Ann)
import           Hyper.Infer (Inferred, infer, iRes)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalDecl, nScheme)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Combinator.Flip (Flip(..))
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
import           Lamdu.Expr.IRef (ValI, ValP)
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
    Definition.Expr (Val a) ->
    PureInfer (Tree (Inferred a UVar) V.Term)

unmemoizedInfer :: InferFunc a
unmemoizedInfer defExpr =
    do
        addDeps <- loadDeps (defExpr ^. Definition.exprFrozenDeps)
        infer (defExpr ^. Definition.expr) & Reader.local addDeps

propEntityId :: Property f (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

preparePayloads ::
    Map NominalId (Tree Pure (NominalDecl T.Type)) ->
    CurAndPrev (EvalResults (ValI m)) ->
    Tree (Ann (ValP m, Tree Pure T.Type, Tree V.IResult UVar)) V.Term ->
    Tree (Ann (Input.Payload m ())) V.Term
preparePayloads nomsMap evalRes inferredVal =
    inferredVal
    & annotations %~ f
    & Input.preparePayloads
    & Input.initLocalsInScope []
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
              , Input._evalResults = evalRes <&> exprEvalRes typ execId
              , Input._userData = ()
              }
            )
            where
                eId = propEntityId valIProp
                execId = Property.value valIProp
        exprEvalRes typ pl r =
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
    Monad m => ValP m -> T m (Val (ValP m))
readValAndAddProperties prop =
    ExprIRef.readVal (prop ^. Property.pVal)
    <&> annotations %~ (, ())
    <&> ExprIRef.addProperties (prop ^. Property.pSet)
    <&> annotations %~ fst

data InferOut m = InferOut
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irCtx :: InferState
    }
Lens.makeLenses ''InferOut

resolve ::
    Val (ValP m, Tree V.IResult UVar) ->
    PureInfer (Tree (Ann (ValP m, Tree Pure T.Type, Tree V.IResult UVar)) V.Term)
resolve =
    annotations f
    where
        f ::
            (ValP m, Tree V.IResult UVar) ->
            PureInfer (ValP m, Tree Pure T.Type, Tree V.IResult UVar)
        f (stored, inferred) =
            inferred ^. V.iType & applyBindings
            <&> \x -> (stored, x, inferred)

runInferResult ::
    Monad m =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    PureInfer (Tree (Inferred (ValP m) UVar) V.Term) ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
runInferResult _monitors evalRes act =
    -- TODO: use _monitors
    case runPureInfer V.emptyScope (InferState emptyPureInferState varGen) act of
    Left x -> Left x & pure
    Right (inferredTerm, inferState0) ->
        ParamList.loadForLambdas iterm
        >>=
        \doLoad ->
        case runPureInfer V.emptyScope inferState0 doLoad of
        Left x -> Left x & pure
        Right (_, inferState1) ->
            case resolve iterm & runPureInfer V.emptyScope inferState1 of
            Left x -> Left x & pure
            Right (resolvedTerm, _inferState2) ->
                resolvedTerm ^.. annotations . _2 . ExprLens.tIds
                & makeNominalsMap
                <&>
                \nomsMap ->
                InferOut
                (preparePayloads nomsMap evalRes resolvedTerm & annotations %~ setUserData)
                inferState1
                & Right
        where
            iterm = inferredTerm ^. ExprLens.itermAnn
    where
        setUserData pl = pl & Input.userData %~ \() -> [pl ^. Input.entityId]

inferDef ::
    Monad m =>
    InferFunc (ValP m) -> Debug.Monitors ->
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValP m)) -> V.Var ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
inferDef inferFunc monitors results defExpr defVar =
    do
        defTv <- newUnbound
        inferredVal <-
            inferFunc defExpr
            & Reader.local (V.scopeVarTypes . Lens.at defVar ?~ MkFlip (GMono defTv))
        inferredVal <$ unify defTv (inferredVal ^. iRes . V.iType)
    & runInferResult monitors results

inferDefExpr ::
    Monad m =>
    InferFunc (ValP m) -> Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValP m)) ->
    T m (Either (Tree Pure T.TypeError) (InferOut m))
inferDefExpr inferFunc monitors results defExpr =
    inferFunc defExpr & runInferResult monitors results
