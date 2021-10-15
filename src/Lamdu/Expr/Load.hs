{-# LANGUAGE TypeFamilies #-}
module Lamdu.Expr.Load
    ( def, defExpr, expr, nominal, writeNominal
    ) where

import qualified Data.Property as Property
import           Hyper
import           Hyper.Syntax.Nominal (NominalDecl)
import           Hyper.Syntax.Scheme (QVars)
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

expr :: Monad m => HRef m # Term -> T m (Ann (HRef m) # Term)
expr (HRef valI writeRoot) =
    ExprIRef.readRecursively valI
    <&> hflipped %~ hmap (const (:*: Const ()))
    <&> ExprIRef.toHRefs writeRoot
    <&> hflipped %~ hmap (const (^. _1))

defExprH ::
    Monad m =>
    (ValI m -> T m ()) -> Definition.Expr (ValI m) ->
    T m (Definition.Expr (Ann (HRef m) # Term))
defExprH setExpr loaded = loaded & Definition.expr %%~ expr . (`HRef` setExpr)

defExpr ::
    Monad m =>
    Property.MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m (Definition.Expr (Ann (HRef m) # Term))
defExpr mkProp =
    do
        loaded <- mkProp ^. Property.mkProperty <&> Property.value
        defExprH (Property.modP mkProp . setExpr) loaded
    where
        setExpr e val = val & Definition.expr .~ e

def :: Monad m => DefI m -> T m (Definition (Ann (HRef m) # Term) (DefI m))
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
    T.NominalId -> T m (Either (T.Types # QVars) (Pure # NominalDecl T.Type))
nominal = Transaction.readIRef . ExprIRef.nominalI

-- TODO: Remove this, and when Nominal becomes IRef-based like the
-- rest of the code, use a loader with properties as annotations
writeNominal ::
    Monad m =>
    T.NominalId -> Either (T.Types # QVars) (Pure # NominalDecl T.Type) -> T m ()
writeNominal = Transaction.writeIRef . ExprIRef.nominalI
