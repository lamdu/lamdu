-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Load
    ( assertInferSuccess
    , inferDef
    , inferCheckDef
    , inferCheckDefExpr
    , inferDefExpr
    , loadInferPrepareInput
    , readValAndAddProperties
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types (EntityId)
import           Revision.Deltum.Property (Property)
import qualified Revision.Deltum.Property as Property
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
    Monad m => ValIProperty m -> T m (Val (ValIProperty m))
readValAndAddProperties prop =
    ExprIRef.readVal (prop ^. Property.pVal)
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties (prop ^. Property.pSet)
    <&> fmap fst

inferDef ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    V.Var ->
    T m (Either Infer.Error (Val (Input.Payload m [EntityId]), Infer.Context))
inferDef results defExpr defVar =
    inferDefExprWithRecursiveRef defExpr defVar
    >>= loadInferPrepareInput results
    & InferT.run

inferDefExprHelper ::
    Monad m => Definition.Expr (Val a) -> InferT.M m (Val (Infer.Payload, a))
inferDefExprHelper defExpr =
    Infer.infer (defExpr ^. Definition.exprFrozenDeps)
    Infer.emptyScope (defExpr ^. Definition.expr)
    & InferT.liftInfer

inferDefExpr ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    T m (Either Infer.Error (Val (Input.Payload m [EntityId]), Infer.Context))
inferDefExpr results defExpr =
    inferDefExprHelper defExpr
    >>= loadInferPrepareInput results
    & InferT.run

inferCheckDef ::
    Monad m =>
    Definition.Expr (Val (ValI m)) -> V.Var ->
    T m (Either Infer.Error (Val (Infer.Payload, ValI m), Infer.Context))
inferCheckDef defExpr defVar =
    inferDefExprWithRecursiveRef defExpr defVar
    >>= ParamList.loadForLambdas
    & InferT.run

inferCheckDefExpr ::
    Monad m =>
    Definition.Expr (Val (ValI m)) ->
    T m (Either Infer.Error (Val (Infer.Payload, ValI m), Infer.Context))
inferCheckDefExpr defExpr =
    inferDefExprHelper defExpr
    >>= ParamList.loadForLambdas
    & InferT.run
