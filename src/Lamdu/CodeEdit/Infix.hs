module Lamdu.CodeEdit.Infix(isInfixName, isInfixVar, infixOp) where

import Control.Lens ((^?))
import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, getP)
import qualified Data.Char as Char
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.IRef as ExprIRef

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

isInfixVar :: MonadA m => Expr.VariableRef (ExprIRef.DefI (Tag m)) -> Transaction m Bool
isInfixVar =
  fmap isInfixName . getP .
  Anchors.assocNameRef . ExprIRef.variableRefGuid

infixOp
  :: MonadA m
  => ExprIRef.ExpressionM m ref
  -> Transaction m (Maybe (Expr.VariableRef (ExprIRef.DefI (Tag m))))
infixOp expr =
  -- TODO: use Lens.action here?
  case expr ^? ExprLens.exprGetVariable of
  Just var -> do
    isInfix <- isInfixVar var
    return $ if isInfix then Just var else Nothing
  Nothing -> return Nothing
