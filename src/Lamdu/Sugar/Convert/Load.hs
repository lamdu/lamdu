-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, TupleSections, TypeApplications #-}
module Lamdu.Sugar.Convert.Load
    ( InferResult(..), irVal, irCtx
    , inferDef
    , inferDefExpr
    , makeNominalsMap
    , readValAndAddProperties
    , InferFunc, unmemoizedInfer
    ) where

import           AST (Tree, Pure(..), _Pure, annotations, Ann)
import           AST.Infer (ITerm, iType, irType, infer, IResult)
import           AST.Term.FuncType (FuncType(..))
import           AST.Term.Nominal (NominalDecl, nScheme)
import           AST.Term.Scheme (sTyp)
import           AST.Unify (unify)
import           AST.Unify.Apply (applyBindings)
import           AST.Unify.Binding (UVar)
import           AST.Unify.Generalize (GTerm(..))
import           AST.Unify.New (newUnbound)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
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
    PureInfer (Tree (ITerm a UVar) V.Term)

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
    Tree (Ann (ValP m, Tree Pure T.Type, IResult UVar V.Term)) V.Term ->
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
                MkPure (T.TFun (FuncType paramType _)) ->
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

data InferResult m = InferResult
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irCtx :: InferState
    }
Lens.makeLenses ''InferResult

resolve ::
    Val (ValP m, IResult UVar V.Term) ->
    PureInfer (Tree (Ann (ValP m, Tree Pure T.Type, IResult UVar V.Term)) V.Term)
resolve =
    annotations f
    where
        f (stored, inferred) =
            inferred ^. irType & applyBindings
            <&> \x -> (stored, x, inferred)

runInferResult ::
    Monad m =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    PureInfer (Tree (ITerm (ValP m) UVar) V.Term) ->
    T m (Either (Tree Pure T.TypeError) (InferResult m))
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
                InferResult
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
    T m (Either (Tree Pure T.TypeError) (InferResult m))
inferDef inferFunc monitors results defExpr defVar =
    do
        defTv <- newUnbound
        inferredVal <-
            inferFunc defExpr
            & Reader.local (V.scopeVarTypes . Lens.at defVar ?~ GMono defTv)
        inferredVal <$ unify defTv (inferredVal ^. iType)
    & runInferResult monitors results

inferDefExpr ::
    Monad m =>
    InferFunc (ValP m) -> Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValP m)) ->
    T m (Either (Tree Pure T.TypeError) (InferResult m))
inferDefExpr inferFunc monitors results defExpr =
    inferFunc defExpr & runInferResult monitors results
