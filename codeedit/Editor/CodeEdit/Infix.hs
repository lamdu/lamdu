module Editor.CodeEdit.Infix(isInfixName, isInfixVar, infixOp)
where

import Control.Monad (liftM)
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
