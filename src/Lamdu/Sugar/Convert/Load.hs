-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Load
    ( assertInferSuccess
    , inferDef
    , inferCheckDef
    , inferDefExpr
    , inferRecursive
    , loadInferPrepareInput
    , readValAndAddProperties
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (StateT(..))
import           Data.CurAndPrev (CurAndPrev)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types (EntityId)
import           Text.PrettyPrint.HughesPJClass (pPrint)

import           Lamdu.Prelude

type T = Transaction

assertInferSuccess :: Either Infer.Error a -> a
assertInferSuccess = either (error . ("Type inference failed: " ++) . show . pPrint) id

inferDefExpr :: Infer.Scope -> Definition.Expr (Val a) -> Infer (Val (Infer.Payload, a))
inferDefExpr scope defExpr =
    Infer.infer (defExpr ^. Definition.exprFrozenDeps)
    scope (defExpr ^. Definition.expr)

inferRecursive ::
    Definition.Expr (Val a) -> V.Var -> Infer (Val (Infer.Payload, a))
inferRecursive defExpr defId =
    do
        defTv <- Infer.freshInferredVar Infer.emptyScope "r"
        let scope = Infer.insertTypeOf defId defTv Infer.emptyScope
        inferredVal <- inferDefExpr scope defExpr
        let inferredType = inferredVal ^. Val.payload . _1 . Infer.plType
        unify inferredType defTv
        Update.inferredVal inferredVal & Update.liftInfer

propEntityId :: Property f (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

preparePayloads ::
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    Val (Input.Payload m ())
preparePayloads evalRes inferredVal =
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
              , Input._evalResults = evalRes <&> exprEvalRes execId
              , Input._userData = ()
              }
            )
            where
                eId = propEntityId valIProp
                execId = Property.value valIProp
        exprEvalRes pl r =
            Input.EvalResultsForExpr
            (r ^. erExprValues . Lens.at pl . Lens._Just)
            (r ^. erAppliesOfLam . Lens.at pl . Lens._Just)

loadInferPrepareInput ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    InferT.M (T m) (Val (Input.Payload m [EntityId]))
loadInferPrepareInput evalRes val =
    preparePayloads evalRes val
    <&> setUserData
    & ParamList.loadForLambdas
    where
        setUserData pl =
            pl & Input.userData %~ \() -> [pl ^. Input.entityId]

readValAndAddProperties ::
    Monad m => (ValI m -> T m ()) -> ValI m -> T m (Val (ValIProperty m))
readValAndAddProperties setRoot valI =
    ExprIRef.readVal valI
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties setRoot
    <&> fmap fst

inferDef ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    V.Var ->
    T m (Either Infer.Error (Val (Input.Payload m [EntityId]), Infer.Context))
inferDef results defExpr defVar =
    inferRecursive defExpr defVar
    & InferT.liftInfer
    >>= loadInferPrepareInput results
    & InferT.run

inferCheckDef ::
    Definition.Expr (Val a) -> V.Var ->
    Either Infer.Error (Val (Infer.Payload, a), Infer.Context)
inferCheckDef defExpr defVar =
    inferRecursive defExpr defVar
    & Infer.run
    & (`runStateT` Infer.initialContext)
