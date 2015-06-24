{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Lamdu.Expr.Load
    ( loadDef
    ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val(..))

type T = Transaction

loadDefExpr ::
    MonadA m =>
    (Definition.Expr (ExprIRef.ValI m) -> T m ()) ->
    Definition.Expr (ExprIRef.ValI m) -> T m (Definition.Expr (Val (ExprIRef.ValIProperty m)))
loadDefExpr writeDefExpr (Definition.Expr valI exprType) =
    ExprIRef.readVal valI
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties (writeDefExpr . wrap)
    <&> fmap fst
    <&> wrap
    where
        wrap :: val -> Definition.Expr val
        wrap = (`Definition.Expr` exprType)

loadDef :: MonadA m => DefI m -> T m (Definition (Val (ExprIRef.ValIProperty m)) (DefI m))
loadDef defI =
    do
        defBody <- Transaction.readIRef defI
        (`Definition` defI) <$>
            case defBody of
            Definition.BodyExpr expr ->
                Definition.BodyExpr <$>
                loadDefExpr (Transaction.writeIRef defI . Definition.BodyExpr) expr
            Definition.BodyBuiltin bi -> return $ Definition.BodyBuiltin bi
