{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

loadGlobalType :: Monad m => V.Var -> Transaction m (Maybe Scheme)
loadGlobalType globId =
    Transaction.readIRef (ExprIRef.defI globId)
    <&>
    \case
    Definition.BodyExpr defExpr ->
        case defExpr ^. Definition.exprType of
        Definition.ExportedType t -> Just t
        Definition.NoExportedType -> Nothing
    Definition.BodyBuiltin b -> Just (Definition.bType b)

acceptNewType ::
    Monad m =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m -> Scheme ->
    Transaction m ()
acceptNewType defExpr defI inferredType =
    do
        usedDefs <- mapM loadGlobType usedGlobals <&> concat <&> Map.fromList
        Definition.BodyExpr
            Definition.Expr
            { Definition._expr =
                defExpr ^. Definition.expr . Val.payload . Input.stored . Property.pVal
            , Definition._exprType = Definition.ExportedType inferredType
            , Definition._exprFrozenDeps =
                Infer.Deps
                { Infer._depsGlobalTypes = usedDefs
                , Infer._depsNominals = mempty -- TODO
                }
            }
            & Transaction.writeIRef defI
    where
        usedGlobals = defExpr ^.. Definition.expr . ExprLens.valGlobals (Set.singleton (ExprIRef.globalId defI))
        loadGlobType globId =
            loadGlobalType globId
            <&> Lens._Just %~ (,) globId <&> (^.. Lens._Just)

makeExprDefTypeInfo ::
    Monad m =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionTypeInfo m)
makeExprDefTypeInfo defExpr defI =
    do
        sugarContext <- ConvertM.readContext
        let inferContext = sugarContext ^. ConvertM.scInferContext
        let inferredType =
                defExpr ^. Definition.expr . Val.payload . Input.inferredType
                & Infer.makeScheme inferContext
        case defExpr ^. Definition.exprType of
            Definition.ExportedType defType
                | defType `Scheme.alphaEq` inferredType ->
                    DefinitionExportedTypeInfo defType
            defType ->
                DefinitionNewType AcceptNewType
                { antOldExportedType = defType
                , antNewInferredType = inferredType
                , antAccept = acceptNewType defExpr defI inferredType
                }
            & return

convert ::
    (Monoid a, Monad m) =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody UUID m (ExpressionU m a))
convert defExpr defI =
    do
        content <-
            ConvertBinder.convertDefinitionBinder defI (defExpr ^. Definition.expr)
        typeInfo <- makeExprDefTypeInfo defExpr defI
        return $ DefinitionBodyExpression DefinitionExpression
            { _deContent = content
            , _deTypeInfo = typeInfo
            }
