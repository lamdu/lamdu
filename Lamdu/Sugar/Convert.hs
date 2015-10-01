{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert
    ( convertDefI
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import           Lamdu.Sugar.Convert.Infer (loadInfer)
import           Lamdu.Sugar.Convert.Monad (Context(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Text.PrettyPrint.HughesPJClass (pPrint)

import           Prelude.Compat

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

mkContext ::
    MonadA m =>
    DefI m -> Anchors.Code (Transaction.MkProperty m) m -> Infer.Context ->
    Context m
mkContext defI cp inferContext =
    Context
    { _scInferContext = inferContext
    , _scDefI = defI
    , _scCodeAnchors = cp
    , _scTagParamInfos = mempty
    , _scMExtractDestPos = Nothing
    , _scNullParams = mempty
    , _scReinferCheckRoot =
          do
              defBody <- Transaction.readIRef defI
              case defBody of
                  Definition.BodyBuiltin {} -> return True
                  Definition.BodyExpr (Definition.Expr valI _) ->
                      ExprIRef.readVal valI
                      <&> fmap (flip (,) ())
                      <&> ExprIRef.addProperties (error "TODO: DefExpr root setIRef")
                      <&> fmap fst
                      >>= -- TODO: loadInfer is for sugar, we don't need sugar here
                          loadInfer mempty
                      <&> Lens.has Lens._Right
    , scConvertSubexpression = ConvertExpr.convert
    }

convertDefI ::
    MonadA m =>
    CurAndPrev (EvalResults (ExprIRef.ValI m)) ->
    Anchors.CodeProps m ->
    Definition.Definition (Val (ExprIRef.ValIProperty m)) (DefI m) ->
    Transaction m (DefinitionU m [EntityId])
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
                    loadInfer evalRes val
                    <&> either (error . ("Type inference failed: " ++) . show . pPrint) id
                let exprI = val ^. V.payload . Property.pVal
                ConvertDefExpr.convert exprI (Definition.Expr valInferred typ) defI
                    & ConvertM.run (mkContext defI cp newInferContext)
