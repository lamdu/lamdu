-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections #-}
module Lamdu.Sugar.Convert.Load
    ( assertInferSuccess
    , InferResult(..), irVal, irCtx
    , inferDef
    , inferDefExpr
    , makeNominalsMap
    , loadInferPrepareInput
    , readValAndAddProperties
    , InferFunc, unmemoizedInfer
    ) where

import           AST (ann, annotations)
import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import           Control.Monad.Transaction (transaction)
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update
import           Lamdu.Sugar.Convert.Input (EvalResultsForExpr(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import           Text.PrettyPrint.HughesPJClass (pPrint)

import           Lamdu.Prelude

type T = Transaction

assertInferSuccess :: HasCallStack => Either Infer.Error a -> a
assertInferSuccess = either (error . ("Type inference failed: " ++) . show . pPrint) id

type InferFunc a =
    Definition.Expr (Val a) -> Infer.Scope -> Infer (Val (Infer.Payload, a))

unmemoizedInfer :: InferFunc a
unmemoizedInfer defExpr scope =
    Infer.infer (defExpr ^. Definition.exprFrozenDeps) scope
    (defExpr ^. Definition.expr)

propEntityId :: Property f (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

preparePayloads ::
    Map NominalId N.Nominal ->
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValP m) ->
    Val (Input.Payload m ())
preparePayloads nomsMap evalRes inferredVal =
    inferredVal
    & annotations %~ f
    & Input.preparePayloads
    & Input.initLocalsInScope []
    where
        f (inferPl, valIProp) =
            ( eId
            , \varRefs ->
              Input.Payload
              { Input._varRefsOfLambda = varRefs
              , Input._entityId = eId
              , Input._localsInScope = []
              , Input._stored = valIProp
              , Input._inferred = inferPl
              , Input._evalResults = evalRes <&> exprEvalRes typ execId
              , Input._userData = ()
              }
            )
            where
                typ = inferPl ^. Infer.plType
                eId = propEntityId valIProp
                execId = Property.value valIProp
        exprEvalRes typ pl r =
            EvalResultsForExpr
            { _eResults = r ^. erExprValues . Lens.ix pl <&> addTypes nomsMap typ
            , _eAppliesOfLam =
                case typ of
                T.TFun paramType _ ->
                    r ^. erAppliesOfLam . Lens.ix pl
                    <&> Lens.mapped . Lens.mapped %~ addTypes nomsMap paramType
                _ | r ^. erAppliesOfLam & Map.null -> Map.empty
                  | otherwise -> error "Have non-empty erAppliesOfLam for non-function-type"
            }

makeNominalsMap :: Monad m => [T.Type] -> T m (Map NominalId N.Nominal)
makeNominalsMap types =
    traverse_ loadForType types
    & (`State.execStateT` mempty)
    <&> Map.mapMaybe id
    where
        loadForType typ = typ ^.. ExprLens.typeTIds & traverse_ loadForTid
        loadForTid tid =
            do
                loaded <- State.get
                unless (Map.member tid loaded) $
                    do
                        nom <- ExprLoad.nominal tid & lift
                        Map.insert tid nom loaded & State.put
                        nom ^.. Lens._Just . N.nomType . Scheme.schemeType
                            & traverse_ loadForType

loadInferPrepareInput ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValP m) ->
    InferT.M (T m) (Val (Input.Payload m [EntityId]))
loadInferPrepareInput evalRes x =
    do
        nomsMap <-
            x ^.. annotations . _1 . Infer.plType
            & makeNominalsMap & transaction
        preparePayloads nomsMap evalRes x
            & annotations %~ setUserData
            & ParamList.loadForLambdas
    where
        setUserData pl =
            pl & Input.userData %~ \() -> [pl ^. Input.entityId]

readValAndAddProperties ::
    Monad m => ValP m -> T m (Val (ValP m))
readValAndAddProperties prop =
    ExprIRef.readVal (prop ^. Property.pVal)
    <&> annotations %~ (, ())
    <&> ExprIRef.addProperties (prop ^. Property.pSet)
    <&> annotations %~ fst

data InferResult m = InferResult
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irCtx :: Infer.Context
    }
Lens.makeLenses ''InferResult

runInferResult ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Infer (Val (Infer.Payload, ValP m)) ->
    T m (Either Infer.Error (InferResult m))
runInferResult monitors results act =
    act
    & InferT.liftInfer
    >>= loadInferPrepareInput results
    & InferT.run monitors
    <&> fmap toResult
    where
        toResult (x, ctx) = InferResult x ctx

inferDef ::
    (HasCallStack, Monad m) =>
    InferFunc (ValP m) -> Debug.Monitors ->
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValP m)) -> V.Var ->
    T m (Either Infer.Error (InferResult m))
inferDef infer monitors results defExpr defVar =
    do
        defTv <- Infer.freshInferredVar Infer.emptyScope "r"
        let scope = Infer.insertTypeOf defVar defTv Infer.emptyScope
        inferredVal <- infer defExpr scope
        let inferredType = inferredVal ^. ann . _1 . Infer.plType
        unify inferredType defTv
        Update.inferredVal inferredVal & Update.liftInfer
    & runInferResult monitors results

inferDefExpr ::
    (HasCallStack, Monad m) =>
    InferFunc (ValP m) -> Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValP m)) ->
    T m (Either Infer.Error (InferResult m))
inferDefExpr infer monitors results defExpr =
    infer defExpr Infer.emptyScope & runInferResult monitors results
