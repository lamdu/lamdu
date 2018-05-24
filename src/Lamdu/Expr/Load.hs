{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( def, defExprProperty, expr, nominal
    ) where

import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import           Lamdu.Calc.Val.Annotated (Val(..))
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

expr :: Monad m => ValP m -> T m (Val (ValP m))
expr (Property valI writeRoot) =
    ExprIRef.readVal valI
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties writeRoot
    <&> fmap fst

defExpr ::
    Monad m =>
    (ValI m -> T m ()) -> Definition.Expr (ValI m) ->
    T m (Definition.Expr (Val (ValP m)))
defExpr setExpr loaded = loaded & Definition.expr %%~ expr . (`Property` setExpr)

defExprProperty ::
    Monad m =>
    Property.MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m (Definition.Expr (Val (ValP m)))
defExprProperty mkProp =
    do
        loaded <- mkProp ^. Property.mkProperty <&> Property.value
        defExpr setExpr loaded
    where
        setExpr e =
            do
                prop <- mkProp ^. Property.mkProperty
                prop ^. Property.pVal
                    & Definition.expr .~ e
                    & Property.set prop

def :: Monad m => DefI m -> T m (Definition (Val (ValP m)) (DefI m))
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
