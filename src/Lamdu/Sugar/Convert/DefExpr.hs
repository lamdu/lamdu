{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

acceptNewType :: Monad m => DefI m -> Scheme -> Transaction m ()
acceptNewType defI inferredType =
    Transaction.readIRef defI
    <&> Definition.defType .~ inferredType
    >>= Transaction.writeIRef defI

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
                , antAccept = acceptNewType defI inferredType
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
