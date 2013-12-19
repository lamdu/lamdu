{-# LANGUAGE DeriveFunctor #-}
module Lamdu.Data.Ops
  ( newHole, wrap, setToWrapper
  , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
  , addListItem
  , newPublicDefinition
  , newDefinition, presentationModeOfName
  , savePreJumpPosition, jumpBack
  , newPane
  , newClipboard
  , makeNewTag, makeNewPublicTag
  , isInfix
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Lens.Operators
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, getP, setP, modP)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Data.Expr.IRef (DefIM)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil

type T = Transaction

setToWrapper ::
  MonadA m =>
  ExprIRef.ExprI (Tag m) ->
  ExprIRef.ExprProperty m ->
  T m (ExprIRef.ExprI (Tag m))
setToWrapper wrappedI destP = do
  newFuncI <- newHole
  destI <$ ExprIRef.writeExprBody destI (ExprUtil.makeApply newFuncI wrappedI)
  where
    destI = Property.value destP

wrap ::
  MonadA m =>
  ExprIRef.ExprProperty m ->
  T m (ExprIRef.ExprI (Tag m))
wrap exprP = do
  newFuncI <- newHole
  applyI <-
    ExprIRef.newExprBody .
    ExprUtil.makeApply newFuncI $ Property.value exprP
  Property.set exprP applyI
  return applyI

newHole :: MonadA m => T m (ExprIRef.ExprI (Tag m))
newHole = ExprIRef.newExprBody $ Expr.BodyLeaf Expr.Hole

replace ::
  MonadA m =>
  ExprIRef.ExprProperty m ->
  ExprIRef.ExprI (Tag m) ->
  T m (ExprIRef.ExprI (Tag m))
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole :: MonadA m => ExprIRef.ExprProperty m -> T m (ExprIRef.ExprI (Tag m))
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: MonadA m => ExprIRef.ExprProperty m -> T m (ExprIRef.ExprI (Tag m))
setToHole exprP =
  exprI <$ ExprIRef.writeExprBody exprI hole
  where
    hole = Expr.BodyLeaf Expr.Hole
    exprI = Property.value exprP

lambdaWrap
  :: MonadA m
  => ExprIRef.ExprProperty m
  -> T m (Guid, ExprIRef.ExprI (Tag m))
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    ExprIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: MonadA m
  => ExprIRef.ExprProperty m
  -> T m (Guid, ExprIRef.ExprI (Tag m))
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    ExprIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    ExprIRef.newExprBody $ ExprUtil.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)

addListItem ::
  MonadA m =>
  Anchors.SpecialFunctions (Tag m) ->
  ExprIRef.ExprProperty m ->
  T m (ExprIRef.ExprI (Tag m), ExprIRef.ExprI (Tag m))
addListItem specialFunctions exprP = do
  consTempI <-
    ExprIRef.newExprBody $ ExprLens.bodyDefinitionRef # Anchors.sfCons specialFunctions
  consI <-
    ExprIRef.newExprBody . ExprUtil.makeApply consTempI =<< newHole
  newItemI <- newHole
  headTag <- ExprIRef.newExprBody $ ExprLens.bodyTag # Anchors.sfHeadTag specialFunctions
  tailTag <- ExprIRef.newExprBody $ ExprLens.bodyTag # Anchors.sfTailTag specialFunctions
  argsI <-
    ExprIRef.newExprBody $ ExprLens.bodyKindedRecordFields Expr.KVal #
      [ (headTag, newItemI)
      , (tailTag, Property.value exprP)
      ]
  newListI <- ExprIRef.newExprBody $ ExprUtil.makeApply consI argsI
  Property.set exprP newListI
  return (newListI, newItemI)

newPane :: MonadA m => Anchors.CodeProps m -> DefIM m -> T m ()
newPane codeProps defI = do
  let panesProp = Anchors.panes codeProps
  panes <- getP panesProp
  when (defI `notElem` panes) $
    setP panesProp $ Anchors.makePane defI : panes

savePreJumpPosition :: MonadA m => Anchors.CodeProps m -> WidgetId.Id -> T m ()
savePreJumpPosition codeProps pos = modP (Anchors.preJumps codeProps) $ (pos :) . take 19

jumpBack :: MonadA m => Anchors.CodeProps m -> T m (Maybe (T m WidgetId.Id))
jumpBack codeProps = do
  preJumps <- getP (Anchors.preJumps codeProps)
  return $
    case preJumps of
    [] -> Nothing
    (j:js) -> Just $ do
      setP (Anchors.preJumps codeProps) js
      return j

isInfix :: String -> Bool
isInfix x = not (null x) && all (`elem` operatorChars) x

presentationModeOfName :: String -> PresentationMode
presentationModeOfName x
  | isInfix x = Infix
  | otherwise = OO

newDefinition ::
  MonadA m => String -> PresentationMode ->
  Definition.Body (ExprIRef.ExprIM m) -> T m (DefIM m)
newDefinition name presentationMode defBody = do
  res <- Transaction.newIRef defBody
  let guid = IRef.guid res
  setP (Anchors.assocNameRef guid) name
  setP (Anchors.assocPresentationMode guid) presentationMode
  return res

newPublicDefinition ::
  MonadA m => Anchors.CodeProps m -> String -> T m (DefIM m)
newPublicDefinition codeProps name = do
  defI <-
    newDefinition name (presentationModeOfName name) =<<
    (Definition.Body . Definition.ContentExpr <$> newHole <*> newHole)
  modP (Anchors.globals codeProps) (defI :)
  return defI

newClipboard ::
  MonadA m => Anchors.CodeProps m ->
  ExprIRef.ExprI (Tag m) ->
  T m (DefIM m)
newClipboard codeProps expr = do
  len <- length <$> getP (Anchors.clipboards codeProps)
  def <- Definition.Body (Definition.ContentExpr expr) <$> newHole
  defI <- newDefinition ("clipboard" ++ show len) OO def
  modP (Anchors.clipboards codeProps) (defI:)
  return defI

makeNewTag :: MonadA m => String -> T m Guid
makeNewTag name = do
  tag <- Transaction.newKey
  tag <$ setP (Anchors.assocNameRef tag) name

makeNewPublicTag :: MonadA m => Anchors.CodeProps m -> String -> T m Guid
makeNewPublicTag codeProps name = do
  tag <- makeNewTag name
  modP (Anchors.tags codeProps) (tag :)
  return tag
