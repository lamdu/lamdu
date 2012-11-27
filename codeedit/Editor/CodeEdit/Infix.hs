module Editor.CodeEdit.Infix(isInfixName, isInfixVar, infixOp) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Store.Transaction (Transaction)
import qualified Data.Char as Char
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

isInfixVar :: Monad m => Data.VariableRef -> Transaction m Bool
isInfixVar =
  liftM isInfixName . Anchors.getP .
  Anchors.assocNameRef . Data.variableRefGuid

infixOp
  :: Monad m
  => Data.Expression ref
  -> Transaction m (Maybe Data.VariableRef)
infixOp expr =
  case expr ^. Data.eValue of
  Data.ExpressionLeaf (Data.GetVariable var) -> do
    isInfix <- isInfixVar var
    return $ if isInfix then Just var else Nothing
  _ -> return Nothing
