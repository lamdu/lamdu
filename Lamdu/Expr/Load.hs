{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( def, expr, exprProperty, nominal
    ) where

import           Lamdu.Prelude

import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import           Lamdu.Calc.Val.Annotated (Val(..))
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef

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
        d <- Transaction.readIRef defI
        let writeExpr e =
                d
                & Definition.defBody .~ Definition.BodyExpr e
                & Transaction.writeIRef defI
        d
            & Definition.defPayload .~ defI
            & Definition.defBody . Definition._BodyExpr %%~ defExpr writeExpr

nominal :: Monad m => T.NominalId -> T m Nominal
nominal tid = Transaction.readIRef iref
    where
        iref = ExprIRef.nominalI tid
