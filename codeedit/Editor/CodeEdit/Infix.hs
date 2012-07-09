module Editor.CodeEdit.Infix(isInfixName, isInfixVar, infixOp)
where

import Control.Monad (liftM)
import Data.Store.Transaction (Transaction)
import qualified Data.Char as Char
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

isInfixVar :: Monad m => Data.VariableRef -> Transaction t m Bool
isInfixVar = liftM isInfixName . Anchors.getP . Anchors.variableNameRef

infixOp
  :: Monad m
  => Data.Expression ref
  -> Transaction t m (Maybe Data.VariableRef)
infixOp (Data.ExpressionGetVariable var) = do
  isInfix <- isInfixVar var
  return $ if isInfix then Just var else Nothing
infixOp _ = return Nothing
