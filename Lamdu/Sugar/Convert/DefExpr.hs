{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Prelude.Compat

makeExprDefTypeInfo ::
    MonadA m =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionTypeInfo m)
makeExprDefTypeInfo defExpr defI =
    do
        sugarContext <- ConvertM.readContext
        let inferContext = sugarContext ^. ConvertM.scInferContext
        let inferredType =
                defExpr ^. Definition.expr . V.payload . Input.inferredType
                & Infer.makeScheme inferContext
        case defExpr ^. Definition.exprType of
            Definition.ExportedType defType
                | defType `Scheme.alphaEq` inferredType ->
                    DefinitionExportedTypeInfo defType
            defType ->
                DefinitionNewType AcceptNewType
                { antOldExportedType = defType
                , antNewInferredType = inferredType
                , antAccept =
                    Definition.Expr
                    { Definition._expr =
                        defExpr ^. Definition.expr . V.payload . Input.stored . Property.pVal
                    , Definition._exprType = Definition.ExportedType inferredType
                    } & Definition.BodyExpr
                    & Transaction.writeIRef defI
                }
            & return

convert ::
    (Monoid a, MonadA m) =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody Guid m (ExpressionU m a))
convert defExpr defI =
    do
        content <-
            ConvertBinder.convertBinder (Just recurseVar) (IRef.guid defI)
            (defExpr ^. Definition.expr)
        typeInfo <- makeExprDefTypeInfo defExpr defI
        return $ DefinitionBodyExpression DefinitionExpression
            { _deContent = content
            , _deTypeInfo = typeInfo
            }
