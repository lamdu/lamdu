{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( loadDef, loadExpr, loadExprProperty, loadNominal
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
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Type as T
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
loadDefExpr writeDefExpr defExpr =
    loadExpr (writeDefExpr . wrap) (defExpr ^. Definition.expr)
    <&> wrap
    where
        wrap :: val -> Definition.Expr val
        wrap v = defExpr & Definition.expr .~ v

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

loadNominal :: MonadA m => T.NominalId -> T m (Maybe Nominal)
loadNominal tid =
    do
        e <- Transaction.irefExists iref
        if e
            then Transaction.readIRef iref <&> Just
            else return Nothing
    where
        iref = ExprIRef.nominalI tid
