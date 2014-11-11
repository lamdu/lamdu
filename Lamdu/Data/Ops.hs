{-# LANGUAGE RecordWildCards #-}
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

import Control.Applicative ((<$>), (<$))
import Control.Lens.Operators
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction, getP, setP, modP)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.IRef (ValTree(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V

type T = Transaction

setToWrapper ::
  MonadA m =>
  ExprIRef.ValI (Tag m) ->
  ExprIRef.ValIProperty m ->
  T m (ExprIRef.ValI (Tag m))
setToWrapper wrappedI destP = do
  newFuncI <- newHole
  destI <$ ExprIRef.writeValBody destI (V.BApp (V.Apply newFuncI wrappedI))
  where
    destI = Property.value destP

wrap ::
  MonadA m =>
  ExprIRef.ValIProperty m ->
  T m (ExprIRef.ValI (Tag m))
wrap exprP = do
  newFuncI <- newHole
  applyI <- ExprIRef.newValBody . V.BApp . V.Apply newFuncI $ Property.value exprP
  Property.set exprP applyI
  return applyI

newHole :: MonadA m => T m (ExprIRef.ValI (Tag m))
newHole = ExprIRef.newValBody $ V.BLeaf V.LHole

replace ::
  MonadA m =>
  ExprIRef.ValIProperty m ->
  ExprIRef.ValI (Tag m) ->
  T m (ExprIRef.ValI (Tag m))
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole :: MonadA m => ExprIRef.ValIProperty m -> T m (ExprIRef.ValI (Tag m))
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: MonadA m => ExprIRef.ValIProperty m -> T m (ExprIRef.ValI (Tag m))
setToHole exprP =
  exprI <$ ExprIRef.writeValBody exprI hole
  where
    hole = V.BLeaf V.LHole
    exprI = Property.value exprP

lambdaWrap
  :: MonadA m
  => ExprIRef.ValIProperty m
  -> T m (V.Var, ExprIRef.ValI (Tag m))
lambdaWrap exprP = do
  (newParam, newExprI) <- ExprIRef.newLambda $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: MonadA m
  => ExprIRef.ValIProperty m
  -> T m (V.Var, ExprIRef.ValI (Tag m))
redexWrap exprP = do
  (newParam, newLambdaI) <- ExprIRef.newLambda $ Property.value exprP
  newValueI <- newHole
  newApplyI <- ExprIRef.newValBody . V.BApp $ V.Apply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)

addListItem ::
  MonadA m =>
  Anchors.SpecialFunctions (Tag m) ->
  ExprIRef.ValIProperty m ->
  T m (ExprIRef.ValI (Tag m), ExprIRef.ValI (Tag m))
addListItem Anchors.SpecialFunctions {..} exprP = do
  newItemI <- newHole
  newListI <- ExprIRef.writeValTree $
    app cons $
    recEx sfHeadTag (ValTreeLeaf newItemI) $
    recEx sfTailTag (ValTreeLeaf (Property.value exprP)) $
    recEmpty
  Property.set exprP newListI
  return (newListI, newItemI)
  where
    v = ValTreeNode
    app f x            = v $ V.BApp $ V.Apply f x
    recEx tag val rest = v $ V.BRecExtend $ V.RecExtend tag val rest
    recEmpty           = v $ V.BLeaf V.LRecEmpty
    cons               = v $ ExprLens.valBodyGlobal # ExprIRef.globalId sfCons

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
  Definition.Body (ExprIRef.ValIM m) -> T m (DefIM m)
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
    ((`Definition.Body` Definition.NoExportedType) . Definition.ContentExpr <$> newHole)
  modP (Anchors.globals codeProps) (defI :)
  return defI

newClipboard ::
  MonadA m => Anchors.CodeProps m ->
  ExprIRef.ValI (Tag m) ->
  T m (DefIM m)
newClipboard codeProps expr = do
  len <- length <$> getP (Anchors.clipboards codeProps)
  let def = Definition.Body (Definition.ContentExpr expr) Definition.NoExportedType
  defI <- newDefinition ("clipboard" ++ show len) OO def
  modP (Anchors.clipboards codeProps) (defI:)
  return defI

makeNewTag :: MonadA m => String -> T m T.Tag
makeNewTag name = do
  tag <- UniqueId.new
  setP (Anchors.assocNameRef tag) name
  return tag

makeNewPublicTag :: MonadA m => Anchors.CodeProps m -> String -> T m T.Tag
makeNewPublicTag codeProps name = do
  tag <- makeNewTag name
  modP (Anchors.tags codeProps) (tag :)
  return tag
