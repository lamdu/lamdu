{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI)
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

makeExprDefTypeInfo ::
    MonadA m => ValI m -> DefI m -> Definition.ExportedType -> Scheme -> DefinitionTypeInfo m
makeExprDefTypeInfo _ _ (Definition.ExportedType defType) inferredType
    | defType `Scheme.alphaEq` inferredType = DefinitionExportedTypeInfo defType
makeExprDefTypeInfo defValI defI defType inferredType =
    DefinitionNewType AcceptNewType
    { antOldExportedType = defType
    , antNewInferredType = inferredType
    , antAccept =
        Transaction.writeIRef defI $
        Definition.BodyExpr $
        Definition.Expr defValI $ Definition.ExportedType inferredType
    }

convert ::
    MonadA m =>
    ValI m -> Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody Guid m (ExpressionU m [EntityId]))
convert exprI (Definition.Expr val defType) defI =
    do
        sugarContext <- ConvertM.readContext
        let inferContext = sugarContext ^. ConvertM.scInferContext
        content <-
            val <&> addStoredEntityIds
            & ConvertBinder.convertBinder (Just recurseVar) defGuid
        return $ DefinitionBodyExpression DefinitionExpression
            { _deContent = content
            , _deTypeInfo =
                makeExprDefTypeInfo exprI defI defType $
                Infer.makeScheme inferContext $
                val ^. V.payload . Input.inferredType
            }
    where
        addStoredEntityIds x =
            x
            & Input.userData .~
                (x ^.. Input.mStored . Lens._Just
                  <&> EntityId.ofValI . Property.value)
        defGuid = IRef.guid defI
