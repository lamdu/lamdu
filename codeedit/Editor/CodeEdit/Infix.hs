module Editor.CodeEdit.Infix(isInfixName, isInfixVar, infixOp) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import qualified Data.Char as Char
import qualified Data.Store.IRef as IRef
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

variableRefGuid :: Data.VariableRef Data.DefinitionIRef -> Guid
variableRefGuid (Data.ParameterRef i) = i
variableRefGuid (Data.DefinitionRef i) = IRef.guid i

isInfixVar :: Monad m => Data.VariableRef Data.DefinitionIRef -> Transaction m Bool
isInfixVar =
  liftM isInfixName . Anchors.getP .
  Anchors.assocNameRef . variableRefGuid

infixOp
  :: Monad m
  => Data.Expression Data.DefinitionIRef ref
  -> Transaction m (Maybe (Data.VariableRef Data.DefinitionIRef))
infixOp expr =
  case expr ^. Data.eValue of
  Data.ExpressionLeaf (Data.GetVariable var) -> do
    isInfix <- isInfixVar var
    return $ if isInfix then Just var else Nothing
  _ -> return Nothing
