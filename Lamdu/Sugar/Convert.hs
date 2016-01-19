{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert
    ( convertDefI, convertExpr
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (unless)
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Store.Guid (Guid)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Nominal as N
import           Lamdu.Expr.Scheme (schemeType)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), OuterScopeInfo(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Text.PrettyPrint.HughesPJClass (pPrint)

import           Prelude.Compat

type T = Transaction

convertDefIBuiltin ::
    MonadA m => Definition.Builtin -> DefI m ->
    DefinitionBody Guid m (ExpressionU m [EntityId])
convertDefIBuiltin (Definition.Builtin name scheme) defI =
    DefinitionBodyBuiltin DefinitionBuiltin
    { _biName = name
    , _biSetName = setName
    , _biType = scheme
    }
    where
        setName =
            Transaction.writeIRef defI .
            Definition.BodyBuiltin . (`Definition.Builtin` scheme)

assertRunInfer :: MonadA m => IRefInfer.M m a -> T m (a, Infer.Context)
assertRunInfer action =
    IRefInfer.run action
    <&> either (error . ("Type inference failed: " ++) . show . pPrint) id

reinferCheckDefinition :: MonadA m => DefI m -> T m Bool
reinferCheckDefinition defI =
    do
        defBody <- Transaction.readIRef defI
        case defBody of
            Definition.BodyBuiltin {} -> return True
            Definition.BodyExpr (Definition.Expr valI _) ->
                ExprIRef.readVal valI
                <&> fmap (flip (,) ())
                <&> ExprIRef.addProperties (error "TODO: DefExpr root setIRef")
                <&> fmap fst
                >>= IRefInfer.run . IRefInfer.loadInferRecursive recurseVar
                <&> Lens.has Lens._Right

reinferCheckExpression :: MonadA m => ValI m -> T m Bool
reinferCheckExpression valI =
    do
        val <- ExprIRef.readVal valI
        IRefInfer.run (IRefInfer.loadInferScope Infer.emptyScope val)
            <&> Lens.has Lens._Right

emptyScopeInfo :: ScopeInfo m
emptyScopeInfo =
    ScopeInfo
      { _siTagParamInfos = mempty
      , _siNullParams = mempty
      , _siLetItems = mempty
      , _siOuter = OuterScopeInfo
        { _osiPos = Nothing
        , _osiVarsUnderPos = []
        }
      }

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
    MonadA m =>
    CurAndPrev (EvalResults (ValI m)) ->
    IRefInfer.M m (Val (Infer.Payload, ValIProperty m)) ->
    T m (Val (Input.Payload m [EntityId]), Infer.Context)
loadInferPrepareInput evalRes action =
    action
    <&> preparePayloads evalRes
    <&> Lens.mapped %~ setUserData
    >>= ParamList.loadForLambdas
    & assertRunInfer
    where
        setUserData pl =
            pl & Input.userData %~ \() -> [pl ^. Input.entityId]

makeNominalsMap ::
    Monad m => Val (Input.Payload m a) -> T m (Map T.NominalId N.Nominal)
makeNominalsMap val =
    mapM_ loadForType (val ^.. Lens.traverse . Input.inferred . Infer.plType)
    & (`State.execStateT` mempty)
    where
        loadForType typ = typ ^.. ExprLens.typeTIds & mapM_ loadForTid
        loadForTid tid =
            do
                loaded <- State.get
                unless (Map.member tid loaded) $
                    do
                        nom <- IRefInfer.loadNominal tid & lift
                        Map.insert tid nom loaded & State.put
                        N.nScheme nom ^. schemeType & loadForType

convertDefI ::
    MonadA m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeProps m ->
    Definition.Definition (Val (ValIProperty m)) (DefI m) ->
    T m (DefinitionU m [EntityId])
convertDefI evalRes cp (Definition.Definition body defI) =
    do
        bodyS <- convertDefBody body
        return Definition
            { _drEntityId = EntityId.ofIRef defI
            , _drName = UniqueId.toGuid defI
            , _drBody = bodyS
            }
    where
        convertDefBody (Definition.BodyBuiltin builtin) =
            return $ convertDefIBuiltin builtin defI
        convertDefBody (Definition.BodyExpr (Definition.Expr val typ)) =
            do
                (valInferred, newInferContext) <-
                    IRefInfer.loadInferRecursive recurseVar val
                    & loadInferPrepareInput evalRes
                nomsMap <- makeNominalsMap valInferred
                let exprI = val ^. V.payload . Property.pVal
                let context =
                        Context
                        { _scInferContext = newInferContext
                        , _scNominalsMap = nomsMap
                        , _scDefI = Just defI
                        , _scCodeAnchors = cp
                        , _scScopeInfo = emptyScopeInfo
                        , _scReinferCheckRoot = reinferCheckDefinition defI
                        , scConvertSubexpression = ConvertExpr.convert
                        }
                ConvertDefExpr.convert exprI (Definition.Expr valInferred typ) defI
                    & ConvertM.run context

convertExpr ::
    MonadA m => CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    Val (ValIProperty m) -> T m (ExpressionU m [EntityId])
convertExpr evalRes cp val =
    do
        (valInferred, newInferContext) <-
            IRefInfer.loadInferScope Infer.emptyScope val
            & loadInferPrepareInput evalRes
        nomsMap <- makeNominalsMap valInferred
        let context =
                Context
                { _scInferContext = newInferContext
                , _scNominalsMap = nomsMap
                , _scDefI = Nothing
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo
                , _scReinferCheckRoot =
                    reinferCheckExpression (val ^. V.payload . Property.pVal)
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertM.convertSubexpression valInferred & ConvertM.run context
