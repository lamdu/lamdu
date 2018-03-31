-- | Load & infer expressions for sugar processing
-- (unify with stored ParamLists, recursion support)
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Lamdu.Sugar.Convert.Load
    ( assertInferSuccess
    , InferResult(..), irVal, irNomsMap, irCtx
    , inferDef
    , inferCheckDef
    , inferCheckDefExpr
    , inferDefExpr
    , loadInferPrepareInput
    , readValAndAddProperties
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
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
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Lamdu.Sugar.Types (EntityId)
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

data InferResult m = InferResult
    { _irVal :: Val (Input.Payload m [EntityId])
    , _irNomsMap :: Map NominalId N.Nominal
    , _irCtx :: Infer.Context
    }
Lens.makeLenses ''InferResult

runInferResult ::
    Monad m =>
    InferT.M (T m) (Val (Input.Payload f [EntityId])) ->
    T m (Either Infer.Error (InferResult f))
runInferResult act =
    InferT.run act >>= traverse toResult
    where
        toResult (val, ctx) =
            makeNominalsMap (val ^.. Lens.folded . Input.inferred . Infer.plType) <&>
            \nomsMap -> InferResult val nomsMap ctx

inferDef ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Definition.Expr (Val (ValIProperty m)) ->
    V.Var ->
    T m (Either Infer.Error (InferResult m))
inferDef results defExpr defVar =
    inferDefExprWithRecursiveRef defExpr defVar
    >>= loadInferPrepareInput results
    & runInferResult

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
    T m (Either Infer.Error (InferResult m))
inferDefExpr results defExpr =
    inferDefExprHelper defExpr
    >>= loadInferPrepareInput results
    & runInferResult

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
