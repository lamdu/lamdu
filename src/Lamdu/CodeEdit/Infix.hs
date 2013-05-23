module Lamdu.CodeEdit.Infix(isInfixName, isInfixVar, infixOp) where

import Control.Lens ((^?))
import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, getP)
import qualified Data.Char as Char
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.IRef as DataIRef

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

isInfixVar :: MonadA m => Expression.VariableRef (DataIRef.DefI (Tag m)) -> Transaction m Bool
isInfixVar =
  fmap isInfixName . getP .
  Anchors.assocNameRef . DataIRef.variableRefGuid

infixOp
  :: MonadA m
  => DataIRef.ExpressionM m ref
  -> Transaction m (Maybe (Expression.VariableRef (DataIRef.DefI (Tag m))))
infixOp expr =
  -- TODO: use Lens.action here?
  case expr ^? ExprLens.exprGetVariable of
  Just var -> do
    isInfix <- isInfixVar var
    return $ if isInfix then Just var else Nothing
  Nothing -> return Nothing
