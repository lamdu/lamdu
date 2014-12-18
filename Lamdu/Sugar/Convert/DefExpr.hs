{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert.DefExpr
  ( convert
  ) where

import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (Context(..))
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

mkContext ::
  MonadA m =>
  DefI m ->
  Anchors.Code (Transaction.MkProperty m) m ->
  Infer.Context -> T m (Context m)
mkContext defI cp inferContext = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scInferContext = inferContext
    , _scDefI = defI
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , _scReinferCheckDefinition =
        do
          defBody <- Transaction.readIRef defI
          case defBody of
            Definition.BodyBuiltin {} -> return True
            Definition.BodyExpr (Definition.Expr valI _) ->
              ExprIRef.readVal valI
              >>= runMaybeT . SugarInfer.loadInfer
              <&> Lens.has Lens._Just
    , scConvertSubexpression = ConvertExpr.convert
    }

makeExprDefTypeInfo ::
  MonadA m => ExprIRef.ValI m -> DefI m -> Definition.ExportedType -> Scheme -> DefinitionTypeInfo m
makeExprDefTypeInfo _ _ (Definition.ExportedType defType) inferredType
  | defType `Scheme.alphaEq` inferredType = DefinitionExportedTypeInfo defType
makeExprDefTypeInfo defValI defI defType inferredType =
  DefinitionNewType AcceptNewType
    { antOldType = defType
    , antNewType = inferredType
    , antAccept =
      Transaction.writeIRef defI $
      Definition.BodyExpr $
      Definition.Expr defValI $ Definition.ExportedType inferredType
    }

convert ::
  MonadA m => Anchors.CodeProps m ->
  Definition.Expr (Val (ExprIRef.ValIProperty m)) ->
  DefI m -> T m (DefinitionBody Guid m (ExpressionU m [EntityId]))
convert cp (Definition.Expr val defType) defI = do
  (valInferred, newInferContext) <-
    SugarInfer.loadInfer val
    <&> _1 . Lens.mapped %~ mkInputPayload
    & runMaybeT
    <&> fromMaybe (error "Type inference failed")
  let
    addStoredEntityIds x =
      x
      & ipData .~
        (x ^.. ipStored . Lens._Just <&> EntityId.ofValI . Property.value)
  context <- mkContext defI cp newInferContext
  ConvertM.run context $ do
    content <-
      valInferred
      <&> addStoredEntityIds
      & ConvertBinder.convertBinder defGuid (ConvertExpr.jumpToDefI cp defI) mempty
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deTypeInfo =
        makeExprDefTypeInfo exprI defI defType $
        Infer.makeScheme newInferContext $
        valInferred ^. V.payload . ipInferred . Infer.plType
      }
  where
    mkInputPayload (inferPl, stored) =
      InputPayload
      { _ipEntityId = EntityId.ofValI $ Property.value stored
      , _ipInferred = inferPl
      , _ipStored = Just stored
      , _ipData = ()
      , _ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      }
    exprI = val ^. V.payload . Property.pVal
    defGuid = IRef.guid defI
