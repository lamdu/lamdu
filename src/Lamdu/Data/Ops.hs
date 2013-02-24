{-# LANGUAGE DeriveFunctor #-}
module Lamdu.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
  , giveAsArgToOperator
  , addListItem
  , makeDefinition
  , newBuiltin, newDefinition
  , savePreJumpPosition, jumpBack
  , newPane
  , newClipboard
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.List.Split (splitOn)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, getP, setP, modP)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type T = Transaction

makeDefinition :: MonadA m => Anchors.CodeProps m -> T m (DefI (Tag m))
makeDefinition codeProps = do
  defI <-
    Transaction.newIRef =<<
    Definition . Definition.BodyExpression <$> newHole <*> newHole
  modP (Anchors.globals codeProps) (defI :)
  return defI

giveAsArg ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.ExpressionI (Tag m))
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (ExprUtil.makeApply newFuncI (Property.value exprP))
  return newFuncI

giveAsArgToOperator ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.ExpressionI (Tag m))
giveAsArgToOperator exprP = do
  op <- newHole
  opApplied <- DataIRef.newExprBody . ExprUtil.makeApply op $ Property.value exprP
  Property.set exprP =<< DataIRef.newExprBody . ExprUtil.makeApply opApplied =<< newHole
  return op

callWithArg ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.ExpressionI (Tag m))
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (ExprUtil.makeApply (Property.value exprP) argI)
  return argI

newHole :: MonadA m => T m (DataIRef.ExpressionI (Tag m))
newHole = DataIRef.newExprBody $ Expression.BodyLeaf Expression.Hole

replace ::
  MonadA m =>
  DataIRef.ExpressionProperty m ->
  DataIRef.ExpressionI (Tag m) ->
  T m (DataIRef.ExpressionI (Tag m))
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole :: MonadA m => DataIRef.ExpressionProperty m -> T m (DataIRef.ExpressionI (Tag m))
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: MonadA m => DataIRef.ExpressionProperty m -> T m (DataIRef.ExpressionI (Tag m))
setToHole exprP =
  exprI <$ DataIRef.writeExprBody exprI hole
  where
    hole = Expression.BodyLeaf Expression.Hole
    exprI = Property.value exprP

lambdaWrap
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> T m (Guid, DataIRef.ExpressionI (Tag m))
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: MonadA m
  => DataIRef.ExpressionProperty m
  -> T m (Guid, DataIRef.ExpressionI (Tag m))
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExprBody $ ExprUtil.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)

addListItem ::
  MonadA m =>
  Anchors.SpecialFunctions (Tag m) ->
  DataIRef.ExpressionProperty m ->
  T m (DataIRef.ExpressionI (Tag m), DataIRef.ExpressionI (Tag m))
addListItem specialFunctions exprP = do
  consTempI <-
    DataIRef.newExprBody . Lens.review ExprUtil.bodyDefinitionRef $ Anchors.sfCons specialFunctions
  consI <-
    DataIRef.newExprBody . ExprUtil.makeApply consTempI =<< newHole
  newItemI <- newHole
  consSectionI <-
    DataIRef.newExprBody $ ExprUtil.makeApply consI newItemI
  newListI <-
    DataIRef.newExprBody . ExprUtil.makeApply consSectionI $
    Property.value exprP
  Property.set exprP newListI
  return (newListI, newItemI)

newPane :: MonadA m => Anchors.CodeProps m -> DefI (Tag m) -> T m ()
newPane codeProps defI = do
  let panesProp = Anchors.panes codeProps
  panes <- getP panesProp
  when (defI `notElem` panes) $
    setP panesProp $ Anchors.makePane defI : panes

savePreJumpPosition :: MonadA m => Anchors.CodeProps m -> Widget.Id -> T m ()
savePreJumpPosition codeProps pos = modP (Anchors.preJumps codeProps) $ (pos :) . take 19

jumpBack :: MonadA m => Anchors.CodeProps m -> T m (Maybe (T m Widget.Id))
jumpBack codeProps = do
  preJumps <- getP (Anchors.preJumps codeProps)
  return $
    case preJumps of
    [] -> Nothing
    (j:js) -> Just $ do
      setP (Anchors.preJumps codeProps) js
      return j

newBuiltin
  :: MonadA m
  => String -> DataIRef.ExpressionI (Tag m)
  -> T m (DefI (Tag m))
newBuiltin fullyQualifiedName typeI =
  newDefinition name . (`Definition` typeI) . Definition.BodyBuiltin .
  Definition.Builtin $ Definition.FFIName (init path) name
  where
    name = last path
    path = splitOn "." fullyQualifiedName

newDefinition ::
  MonadA m => String ->
  DataIRef.DefinitionI (Tag m) -> T m (DefI (Tag m))
newDefinition name defI = do
  res <- Transaction.newIRef defI
  setP (Anchors.assocNameRef (IRef.guid res)) name
  return res

newClipboard ::
  MonadA m => Anchors.CodeProps m ->
  DataIRef.ExpressionI (Tag m) ->
  T m (DefI (Tag m))
newClipboard codeProps expr = do
  len <- length <$> getP (Anchors.clipboards codeProps)
  def <- Definition (Definition.BodyExpression expr) <$> newHole
  defI <- newDefinition ("clipboard" ++ show len) def
  modP (Anchors.clipboards codeProps) (defI:)
  return defI
