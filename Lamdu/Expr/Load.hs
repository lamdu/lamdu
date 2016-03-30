{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( def, expr, exprProperty, nominal
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
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

expr :: Monad m => (ValI m -> T m ()) -> ValI m -> T m (Val (ValIProperty m))
expr writeRoot valI =
    ExprIRef.readVal valI
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties writeRoot
    <&> fmap fst

exprProperty :: Monad m => ValIProperty m -> T m (Val (ValIProperty m))
exprProperty (Property val set) = expr set val

defExpr ::
    Monad m =>
    (Definition.Expr (ValI m) -> T m ()) ->
    Definition.Expr (ValI m) -> T m (Definition.Expr (Val (ValIProperty m)))
defExpr writeDefExpr d =
    expr (writeDefExpr . wrap) (d ^. Definition.expr)
    <&> wrap
    where
        wrap :: val -> Definition.Expr val
        wrap v = d & Definition.expr .~ v

def :: Monad m => DefI m -> T m (Definition (Val (ValIProperty m)) (DefI m))
def defI =
    do
        defBody <- Transaction.readIRef defI
        (`Definition` defI) <$>
            case defBody of
            Definition.BodyExpr e ->
                Definition.BodyExpr <$>
                defExpr (Transaction.writeIRef defI . Definition.BodyExpr) e
            Definition.BodyBuiltin bi -> return $ Definition.BodyBuiltin bi

nominal :: Monad m => T.NominalId -> T m (Maybe Nominal)
nominal tid =
    do
        e <- Transaction.irefExists iref
        if e
            then Transaction.readIRef iref <&> Just
            else return Nothing
    where
        iref = ExprIRef.nominalI tid
