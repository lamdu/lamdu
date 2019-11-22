{-# LANGUAGE TypeFamilies #-}
module Lamdu.Expr.Load
    ( def, defExpr, expr, nominal
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI, HRef(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

expr :: Monad m => Tree (HRef m) Term -> T m (Tree (Ann (HRef m)) Term)
expr (HRef valI writeRoot) =
    ExprIRef.readRecursively valI
    <&> hflipped . hmapped1 %~ (:*: Const ())
    <&> ExprIRef.toHRefs writeRoot
    <&> hflipped . hmapped1 %~ (^. Lens._1)

defExprH ::
    Monad m =>
    (ValI m -> T m ()) -> Definition.Expr (ValI m) ->
    T m (Definition.Expr (Tree (Ann (HRef m)) Term))
defExprH setExpr loaded = loaded & Definition.expr %%~ expr . (`HRef` setExpr)

defExpr ::
    Monad m =>
    Property.MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m (Definition.Expr (Tree (Ann (HRef m)) Term))
defExpr mkProp =
    do
        loaded <- mkProp ^. Property.mkProperty <&> Property.value
        defExprH (Property.modP mkProp . setExpr) loaded
    where
        setExpr e val = val & Definition.expr .~ e

def :: Monad m => DefI m -> T m (Definition (Tree (Ann (HRef m)) Term) (DefI m))
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
