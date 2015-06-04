{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
  ( convert
  ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (Context(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

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
    , _scReinferCheckDefinition =
        do
          defBody <- Transaction.readIRef defI
          case defBody of
            Definition.BodyBuiltin {} -> return True
            Definition.BodyExpr (Definition.Expr valI _) ->
              ExprIRef.readVal valI
              <&> fmap (flip (,) ())
              <&> ExprIRef.addProperties undefined
              <&> fmap fst
              >>= loadInfer
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

loadInfer :: MonadA m =>
  Val (ExprIRef.ValIProperty m) ->
  T m (Maybe (Val (Input.Payload m ()), Infer.Context))
loadInfer val =
  IRefInfer.loadInfer recurseVar val
  <&> _1 . Lens.mapped %~ Input.mkPayload ()
  >>= ParamList.loadForLambdas
  & runMaybeT

convert ::
  MonadA m => Anchors.CodeProps m ->
  Definition.Expr (Val (ExprIRef.ValIProperty m)) ->
  DefI m -> T m (DefinitionBody Guid m (ExpressionU m [EntityId]))
convert cp (Definition.Expr val defType) defI = do
  (valInferred, newInferContext) <-
    loadInfer val <&> fromMaybe (error "Type inference failed")
  context <- mkContext defI cp newInferContext
  ConvertM.run context $ do
    content <-
      valInferred <&> addStoredEntityIds
      & ConvertBinder.convertBinder (Just recurseVar) defGuid
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deTypeInfo =
        makeExprDefTypeInfo exprI defI defType $
        Infer.makeScheme newInferContext $
        valInferred ^. V.payload . Input.inferred . Infer.plType
      }
  where
    addStoredEntityIds x =
      x
      & Input.userData .~
        (x ^.. Input.mStored . Lens._Just
         <&> EntityId.ofValI . Property.value)
    exprI = val ^. V.payload . Property.pVal
    defGuid = IRef.guid defI
