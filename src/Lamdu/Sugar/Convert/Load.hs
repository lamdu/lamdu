-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Lamdu.Sugar.Convert.Load
    ( assertInferSuccess
    , InferResult(..), irVal, irCtx
    , inferDef
    , inferCheckDef
    , inferCheckDefExpr
    , inferDefExpr
    , makeNominalsMap
    , loadInferPrepareInput
    , readValAndAddProperties
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import           Control.Monad.Transaction (transaction)
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as ExprLoad
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

assertInferSuccess :: Either Infer.Error a -> a
assertInferSuccess = either (error . ("Type inference failed: " ++) . show . pPrint) id

inferDefExprWithRecursiveRef ::
    Monad m =>
    Definition.Expr (Val a) -> V.Var -> InferT.M m (Val (Infer.Payload, a))
inferDefExprWithRecursiveRef defExpr defId =
    do
        defTv <- Infer.freshInferredVar Infer.emptyScope "r"
        let scope = Infer.insertTypeOf defId defTv Infer.emptyScope
        inferredVal <-
            Infer.infer (defExpr ^. Definition.exprFrozenDeps) scope
            (defExpr ^. Definition.expr)
        let inferredType = inferredVal ^. Val.payload . _1 . Infer.plType
        unify inferredType defTv
        Update.inferredVal inferredVal & Update.liftInfer
    & InferT.liftInfer

propEntityId :: Property f (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

preparePayloads ::
    Map NominalId N.Nominal ->
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    Val (Input.Payload m ())
preparePayloads nomsMap evalRes inferredVal =
    inferredVal <&> f & Input.preparePayloads
    where
        f (inferPl, valIProp) =
            ( eId
            , \varRefs ->
              Input.Payload
              { Input._varRefsOfLambda = varRefs
              , Input._entityId = eId
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
    mapM_ loadForType types
    & (`State.execStateT` mempty)
    where
        loadForType typ = typ ^.. ExprLens.typeTIds & mapM_ loadForTid
        loadForTid tid =
            do
                loaded <- State.get
                unless (Map.member tid loaded) $
                    do
                        nom <- ExprLoad.nominal tid & lift
                        Map.insert tid nom loaded & State.put
                        nom ^.. N.nomType . N._NominalType . Scheme.schemeType
                            & traverse_ loadForType

loadInferPrepareInput ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    InferT.M (T m) (Val (Input.Payload m [EntityId]))
loadInferPrepareInput evalRes val =
    do
        nomsMap <-
            val ^.. Lens.folded . _1 . Infer.plType
            & makeNominalsMap & transaction
        preparePayloads nomsMap evalRes val
            <&> setUserData
            & ParamList.loadForLambdas
    where
        setUserData pl =
            pl & Input.userData %~ \() -> [pl ^. Input.entityId]

readValAndAddProperties ::
    Monad m => ValIProperty m -> T m (Val (ValIProperty m))
readValAndAddProperties prop =
    ExprIRef.readVal (prop ^. Property.pVal)
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties (prop ^. Property.pSet)
    <&> fmap fst

data InferResult m = InferResult
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irCtx :: Infer.Context
    }
Lens.makeLenses ''InferResult

runInferResult ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    InferT.M (T m) (Val (Infer.Payload, ValIProperty m)) ->
    T m (Either Infer.Error (InferResult m))
runInferResult monitors results act =
    act
    >>= loadInferPrepareInput results
    & InferT.run monitors
    <&> fmap toResult
    where
        toResult (val, ctx) = InferResult val ctx

inferDef ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    V.Var ->
    T m (Either Infer.Error (InferResult m))
inferDef monitors results defExpr defVar =
    inferDefExprWithRecursiveRef defExpr defVar
    & runInferResult monitors results

inferDefExprHelper ::
    Monad m => Definition.Expr (Val a) -> InferT.M m (Val (Infer.Payload, a))
inferDefExprHelper defExpr =
    Infer.infer (defExpr ^. Definition.exprFrozenDeps)
    Infer.emptyScope (defExpr ^. Definition.expr)
    & InferT.liftInfer

inferDefExpr ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    T m (Either Infer.Error (InferResult m))
inferDefExpr monitors results defExpr =
    inferDefExprHelper defExpr
    & runInferResult monitors results

inferCheckDef ::
    Monad m =>
    Debug.Monitors -> Definition.Expr (Val (ValI m)) -> V.Var ->
    T m (Either Infer.Error (Val (Infer.Payload, ValI m), Infer.Context))
inferCheckDef monitors defExpr defVar =
    inferDefExprWithRecursiveRef defExpr defVar
    >>= ParamList.loadForLambdas
    & InferT.run monitors

inferCheckDefExpr ::
    Monad m =>
    Debug.Monitors -> Definition.Expr (Val (ValI m)) ->
    T m (Either Infer.Error (Val (Infer.Payload, ValI m), Infer.Context))
inferCheckDefExpr monitors defExpr =
    inferDefExprHelper defExpr
    >>= ParamList.loadForLambdas
    & InferT.run monitors
