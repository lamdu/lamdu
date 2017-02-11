{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( def, defExprProperty, expr, nominal
    ) where

import           Lamdu.Prelude

import qualified Data.Store.Property as Property
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

defExpr ::
    Monad m =>
    (ValI m -> T m ()) -> Definition.Expr (ValI m) ->
    T m (Definition.Expr (Val (ValIProperty m)))
defExpr setExpr loaded = loaded & Definition.expr %%~ expr setExpr

defExprProperty ::
    Monad m =>
    Transaction.MkProperty m (Definition.Expr (ValI m)) ->
    T m (Definition.Expr (Val (ValIProperty m)))
defExprProperty mkProp =
    do
        loaded <- mkProp ^. Transaction.mkProperty <&> Property.value
        defExpr setExpr loaded
    where
        setExpr e =
            do
                prop <- mkProp ^. Transaction.mkProperty
                prop ^. Property.pVal
                    & Definition.expr .~ e
                    & Property.set prop

def :: Monad m => DefI m -> T m (Definition (Val (ValIProperty m)) (DefI m))
def defI =
    Transaction.readIRef defI
    <&> Definition.defPayload .~ defI
    >>= Definition.defBody . Definition._BodyExpr %%~ defExpr setExpr
    where
        setExpr e =
            Transaction.readIRef defI
            <&> Definition.defBody . Definition._BodyExpr . Definition.expr .~ e
            >>= Transaction.writeIRef defI

nominal :: Monad m => T.NominalId -> T m Nominal
nominal tid = Transaction.readIRef iref
    where
        iref = ExprIRef.nominalI tid
