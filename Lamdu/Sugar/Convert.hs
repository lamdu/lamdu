{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert
  ( convertDefI
  ) where

import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

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

convertDefI ::
  MonadA m => EvalResults (ExprIRef.ValI m) -> Anchors.CodeProps m ->
  Definition.Definition (Val (ExprIRef.ValIProperty m)) (DefI m) ->
  Transaction m (DefinitionU m [EntityId])
convertDefI evalMap cp (Definition.Definition body defI) = do
  bodyS <- convertDefBody body
  return Definition
    { _drEntityId = EntityId.ofIRef defI
    , _drName = UniqueId.toGuid defI
    , _drBody = bodyS
    }
  where
    convertDefBody (Definition.BodyBuiltin builtin) =
      return $ convertDefIBuiltin builtin defI
    convertDefBody (Definition.BodyExpr expr) =
      ConvertDefExpr.convert evalMap cp expr defI
