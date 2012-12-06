{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
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
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewM)
import Editor.Data.IRef (DefI)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Graphics.UI.Bottle.Widget as Widget

type T = Transaction

makeDefinition :: Transaction ViewM DefI
makeDefinition = do
  defI <-
    Transaction.newIRef =<<
    Data.Definition . Data.DefinitionExpression <$> newHole <*> newHole
  Anchors.modP Anchors.globals (defI :)
  return defI

giveAsArg ::
  MonadA m =>
  DataIRef.ExpressionProperty (T m) ->
  T m DataIRef.Expression
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply newFuncI (Property.value exprP))
  return newFuncI

giveAsArgToOperator ::
  MonadA m =>
  DataIRef.ExpressionProperty (T m) ->
  String ->
  T m DataIRef.Expression
giveAsArgToOperator exprP searchTerm = do
  op <- newHole
  (`Property.set` searchTerm) =<< Anchors.assocSearchTermRef (DataIRef.exprGuid op)
  opApplied <- DataIRef.newExprBody . Data.makeApply op $ Property.value exprP
  Property.set exprP =<< DataIRef.newExprBody . Data.makeApply opApplied =<< newHole
  return op

callWithArg ::
  MonadA m =>
  DataIRef.ExpressionProperty (T m) ->
  T m DataIRef.Expression
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply (Property.value exprP) argI)
  return argI

newHole :: MonadA m => T m DataIRef.Expression
newHole = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole

replace
  :: MonadA m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.Expression
  -> T m DataIRef.Expression
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole
  :: MonadA m
  => DataIRef.ExpressionProperty (T m)
  -> T m DataIRef.Expression
replaceWithHole exprP = replace exprP =<< newHole

lambdaWrap
  :: MonadA m
  => DataIRef.ExpressionProperty (T m)
  -> T m (Guid, DataIRef.Expression)
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: MonadA m
  => DataIRef.ExpressionProperty (T m)
  -> T m (Guid, DataIRef.Expression)
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExprBody $ Data.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)

newPane :: DefI -> Transaction ViewM ()
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
  => String -> DataIRef.Expression
  -> Transaction m DefI
newBuiltin fullyQualifiedName typeI =
  newDefinition name . (`Data.Definition` typeI) . Data.DefinitionBuiltin .
  Data.Builtin $ Data.FFIName (init path) name
  where
    name = last path
    path = splitOn "." fullyQualifiedName

newDefinition :: MonadA m => String -> DataIRef.DefinitionI -> Transaction m DefI
newDefinition name defI = do
  res <- Transaction.newIRef defI
  Anchors.setP (Anchors.assocNameRef (IRef.guid res)) name
  return res

newClipboard :: DataIRef.Expression -> Transaction ViewM DefI
newClipboard expr = do
  len <- length <$> Anchors.getP Anchors.clipboards
  def <- Data.Definition (Data.DefinitionExpression expr) <$> newHole
  defI <- newDefinition ("clipboard" ++ show len) def
  Anchors.modP Anchors.clipboards (defI:)
  return defI
