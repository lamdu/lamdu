{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( loadDef, loadExpr, loadExprProperty
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val(..))

type T = Transaction

loadExpr :: MonadA m => (ValI m -> T m ()) -> ValI m -> T m (Val (ValIProperty m))
loadExpr writeRoot valI =
    ExprIRef.readVal valI
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties writeRoot
    <&> fmap fst

loadExprProperty :: MonadA m => ValIProperty m -> T m (Val (ValIProperty m))
loadExprProperty (Property val set) = loadExpr set val

loadDefExpr ::
    MonadA m =>
    (Definition.Expr (ValI m) -> T m ()) ->
    Definition.Expr (ValI m) -> T m (Definition.Expr (Val (ValIProperty m)))
loadDefExpr writeDefExpr (Definition.Expr valI exprType) =
    loadExpr (writeDefExpr . wrap) valI
    <&> wrap
    where
        wrap :: val -> Definition.Expr val
        wrap = (`Definition.Expr` exprType)

loadDef :: MonadA m => DefI m -> T m (Definition (Val (ValIProperty m)) (DefI m))
loadDef defI =
    do
        defBody <- Transaction.readIRef defI
        (`Definition` defI) <$>
            case defBody of
            Definition.BodyExpr expr ->
                Definition.BodyExpr <$>
                loadDefExpr (Transaction.writeIRef defI . Definition.BodyExpr) expr
            Definition.BodyBuiltin bi -> return $ Definition.BodyBuiltin bi
