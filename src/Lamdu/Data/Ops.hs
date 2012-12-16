{-# LANGUAGE DeriveFunctor #-}
module Lamdu.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  , giveAsArgToOperator
  , makeDefinition
  , newBuiltin, newDefinition
  , savePreJumpPosition, jumpBack
  , newPane
  , newClipboard
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.List.Split (splitOn)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Lamdu.Anchors (ViewM)
import Lamdu.Data.IRef (DefI)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef

type T = Transaction

makeDefinition :: Transaction ViewM (DefI (Tag ViewM))
makeDefinition = do
  defI <-
    Transaction.newIRef =<<
    Data.Definition . Data.DefinitionExpression <$> newHole <*> newHole
  Anchors.modP Anchors.globals (defI :)
  return defI

giveAsArg ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.Expression (Tag m))
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply newFuncI (Property.value exprP))
  return newFuncI

giveAsArgToOperator ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  String ->
  T m (DataIRef.Expression (Tag m))
giveAsArgToOperator exprP searchTerm = do
  op <- newHole
  (`Property.set` searchTerm) =<< Anchors.assocSearchTermRef (DataIRef.exprGuid op)
  opApplied <- DataIRef.newExprBody . Data.makeApply op $ Property.value exprP
  Property.set exprP =<< DataIRef.newExprBody . Data.makeApply opApplied =<< newHole
  return op

callWithArg ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.Expression (Tag m))
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply (Property.value exprP) argI)
  return argI

newHole :: MonadA m => T m (DataIRef.Expression (Tag m))
newHole = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole

replace
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> DataIRef.Expression (Tag m)
  -> T m (DataIRef.Expression (Tag m))
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> T m (DataIRef.Expression (Tag m))
replaceWithHole exprP = replace exprP =<< newHole

lambdaWrap
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> T m (Guid, DataIRef.Expression (Tag m))
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> T m (Guid, DataIRef.Expression (Tag m))
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExprBody $ Data.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)

newPane :: DefI (Tag ViewM) -> Transaction ViewM ()
newPane defI = do
  panesP <- Anchors.panes
  when (defI `notElem` Property.value panesP) $
    Property.set panesP $ Anchors.makePane defI : Property.value panesP

savePreJumpPosition :: Widget.Id -> Transaction ViewM ()
savePreJumpPosition pos = Anchors.modP Anchors.preJumps $ (pos :) . take 19

jumpBack :: Transaction ViewM (Maybe (Transaction ViewM Widget.Id))
jumpBack = do
  preJumpsP <- Anchors.preJumps
  return $
    case Property.value preJumpsP of
    [] -> Nothing
    (j:js) -> Just $ do
      Property.set preJumpsP js
      return j

newBuiltin
  :: MonadA m
  => String -> DataIRef.Expression (Tag m)
  -> Transaction m (DefI (Tag m))
newBuiltin fullyQualifiedName typeI =
  newDefinition name . (`Data.Definition` typeI) . Data.DefinitionBuiltin .
  Data.Builtin $ Data.FFIName (init path) name
  where
    name = last path
    path = splitOn "." fullyQualifiedName

newDefinition ::
  MonadA m => String ->
  DataIRef.DefinitionI (Tag m) -> Transaction m (DefI (Tag m))
newDefinition name defI = do
  res <- Transaction.newIRef defI
  Anchors.setP (Anchors.assocNameRef (IRef.guid res)) name
  return res

newClipboard :: DataIRef.Expression (Tag ViewM) -> Transaction ViewM (DefI (Tag ViewM))
newClipboard expr = do
  len <- length <$> Anchors.getP Anchors.clipboards
  def <- Data.Definition (Data.DefinitionExpression expr) <$> newHole
  defI <- newDefinition ("clipboard" ++ show len) def
  Anchors.modP Anchors.clipboards (defI:)
  return defI
