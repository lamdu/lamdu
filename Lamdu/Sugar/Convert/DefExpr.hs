{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

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

loadGlobalType :: Monad m => V.Var -> Transaction m Scheme
loadGlobalType globId =
    Transaction.readIRef (ExprIRef.defI globId) <&> (^. Definition.defType)

acceptNewType ::
    Monad m =>
    Definition.Expr (Val (Input.Payload m a)) -> DefI m -> Scheme ->
    Transaction m ()
acceptNewType defExpr defI inferredType =
    do
        usedDefs <- mapM loadGlobType usedGlobals <&> Map.fromList
        Transaction.writeIRef defI
            Definition.Definition
            { Definition._defBody =
                Definition.BodyExpr
                Definition.Expr
                { Definition._expr =
                    defExpr ^. Definition.expr . Val.payload . Input.stored . Property.pVal
                , Definition._exprFrozenDeps =
                    Infer.Deps
                    { Infer._depsGlobalTypes = usedDefs
                    , Infer._depsNominals = mempty -- TODO
                    }
                }
            , Definition._defType = inferredType
            , Definition._defPayload = ()
            }
    where
        usedGlobals = defExpr ^.. Definition.expr . ExprLens.valGlobals (Set.singleton (ExprIRef.globalId defI))
        loadGlobType globId = loadGlobalType globId <&> (,) globId

makeExprDefTypeInfo ::
    Monad m =>
    Scheme -> Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionTypeInfo m)
makeExprDefTypeInfo defType defExpr defI =
    do
        sugarContext <- ConvertM.readContext
        let inferContext = sugarContext ^. ConvertM.scInferContext
        let inferredType =
                defExpr ^. Definition.expr . Val.payload . Input.inferredType
                & Infer.makeScheme inferContext
        if Scheme.alphaEq defType inferredType
            then DefinitionExportedTypeInfo defType & return
            else
                DefinitionNewType AcceptNewType
                { antOldExportedType = defType
                , antNewInferredType = inferredType
                , antAccept = acceptNewType defExpr defI inferredType
                }
                & return

convert ::
    (Monoid a, Monad m) =>
    Scheme -> Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody UUID m (ExpressionU m a))
convert defType defExpr defI =
    do
        content <-
            ConvertBinder.convertDefinitionBinder defI (defExpr ^. Definition.expr)
        typeInfo <- makeExprDefTypeInfo defType defExpr defI
        return $ DefinitionBodyExpression DefinitionExpression
            { _deContent = content
            , _deTypeInfo = typeInfo
            }
