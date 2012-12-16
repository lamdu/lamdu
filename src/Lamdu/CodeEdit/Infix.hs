module Lamdu.CodeEdit.Infix(isInfixName, isInfixVar, infixOp) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import qualified Data.Char as Char
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

variableRefGuid :: Data.VariableRef (DataIRef.DefI t) -> Guid
variableRefGuid (Data.ParameterRef i) = i
variableRefGuid (Data.DefinitionRef i) = IRef.guid i

isInfixVar :: MonadA m => Data.VariableRef (DataIRef.DefI (Tag m)) -> Transaction m Bool
isInfixVar =
  fmap isInfixName . Anchors.getP .
  Anchors.assocNameRef . variableRefGuid

infixOp
  :: MonadA m
  => Data.Expression (DataIRef.DefI (Tag m)) ref
  -> Transaction m (Maybe (Data.VariableRef (DataIRef.DefI (Tag m))))
infixOp expr =
  case expr ^. Data.eValue of
  Data.ExpressionLeaf (Data.GetVariable var) -> do
    isInfix <- isInfixVar var
    return $ if isInfix then Just var else Nothing
  _ -> return Nothing
