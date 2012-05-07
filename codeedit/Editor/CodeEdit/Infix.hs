{-# OPTIONS -O2 -Wall #-}
module Editor.CodeEdit.Infix(isInfixName, isInfixVar, isInfixFunc, infixOp, isApplyOfInfixOp, infixFuncOfRArg)
where

import Control.Monad (liftM)
import Data.Maybe (isJust)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import qualified Data.Char as Char
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

isInfixVar :: Monad m => Data.VariableRef -> Transaction t m Bool
isInfixVar = liftM isInfixName . Property.get . Anchors.variableNameRef

infixOp :: Monad m => IRef Data.Expression -> Transaction t m (Maybe Data.VariableRef)
infixOp funcI = do
  expr <- Transaction.readIRef funcI
  case expr of
    Data.ExpressionGetVariable var -> do
      isInfix <- isInfixVar var
      return $ if isInfix then Just var else Nothing
    _ -> return Nothing

isInfixFunc :: Monad m => IRef Data.Expression -> Transaction t m Bool
isInfixFunc = liftM isJust . infixOp

infixFuncOfRArg
  :: Monad m
  => IRef Data.Expression
  -> Transaction t m (Maybe (IRef Data.Expression))
infixFuncOfRArg exprI = do
  expr <- Transaction.readIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> do
      res <- isInfixFunc funcI
      if res
        then return (Just funcI)
        else return Nothing
    _ -> return Nothing

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> Transaction t m Bool
isApplyOfInfixOp = liftM isJust . infixFuncOfRArg
