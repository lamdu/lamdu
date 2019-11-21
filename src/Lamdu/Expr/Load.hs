{-# LANGUAGE TypeFamilies, TupleSections #-}
module Lamdu.Expr.Load
    ( def, defExpr, expr, nominal
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Property as Property
import           Hyper (Tree, Pure, _HFlip, hmapped1)
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Type as T
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
    ExprIRef.readRecursively valI
    <&> Lens.from _HFlip . hmapped1 %~ Const . (, ())
    <&> ExprIRef.addProperties writeRoot
    <&> Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ fst

defExprH ::
    Monad m =>
    (ValI m -> T m ()) -> Definition.Expr (ValI m) ->
    T m (Definition.Expr (Val (ValP m)))
defExprH setExpr loaded = loaded & Definition.expr %%~ expr . (`Property` setExpr)

defExpr ::
    Monad m =>
    Property.MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m (Definition.Expr (Val (ValP m)))
defExpr mkProp =
    do
        loaded <- mkProp ^. Property.mkProperty <&> Property.value
        defExprH (Property.modP mkProp . setExpr) loaded
    where
        setExpr e val = val & Definition.expr .~ e

def :: Monad m => DefI m -> T m (Definition (Val (ValP m)) (DefI m))
def defI =
    Transaction.readIRef defI
    <&> Definition.defPayload .~ defI
    >>= Definition.defBody . Definition._BodyExpr %%~ defExprH setExpr
    where
        setExpr e =
            Transaction.readIRef defI
            <&> Definition.defBody . Definition._BodyExpr . Definition.expr .~ e
            >>= Transaction.writeIRef defI

nominal ::
    Monad m =>
    T.NominalId -> T m (Maybe (Tree Pure (NominalDecl T.Type)))
nominal tid =
    Transaction.irefExists iref
    >>=
    \case
    False -> pure Nothing -- Opaque nominal
    True -> Transaction.readIRef iref <&> Just
    where
        iref = ExprIRef.nominalI tid
