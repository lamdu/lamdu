{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Parens(addTextParens, addParens, makeParensId)
where

import Control.Monad (liftM, liftM2, void)
import Data.ByteString.Char8 (pack)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.Sugar (ParensInfo(..), HighlightParens(..))
import Editor.CTransaction (TWidget, WidgetT, transaction, subCursor)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as A
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

makeCondParensId
  :: Monad m
  => Bool -> A.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe Widget.Id)
makeCondParensId False = const $ return Nothing
makeCondParensId True = liftM Just . makeParensId

makeCondHighlightedTextParens
  :: Monad m => Bool -> A.ExpressionAncestry m -> Transaction ViewTag m (Maybe ParensInfo)
makeCondHighlightedTextParens bool ancestry =
  (liftM . fmap) (TextParens DoHighlightParens) $
  makeCondParensId bool ancestry

makeCondUnhighlightedTextParens
  :: Monad m => Bool -> A.ExpressionAncestry m -> Transaction ViewTag m (Maybe ParensInfo)
makeCondUnhighlightedTextParens bool ancestry =
  (liftM . fmap) (TextParens DontHighlightParens) $
  makeCondParensId bool ancestry

getParensInfo
  :: Monad m
  => Sugar.Expression m -> A.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe ParensInfo)
getParensInfo (Sugar.ExpressionApply _) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyArg A.Prefix _ _) : _) =
  liftM (Just . TextParens DoHighlightParens) $ makeParensId ancestry
getParensInfo (Sugar.ExpressionApply (Sugar.Apply func _)) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyArg _ _ _) : _) = do
  funcI <- Property.get $ Sugar.rExpressionPtr func
  isInfix <-
    liftM2 (||)
    (Infix.isInfixFunc funcI) (Infix.isApplyOfInfixOp funcI)
  makeCondHighlightedTextParens isInfix ancestry
getParensInfo (Sugar.ExpressionApply (Sugar.Apply func _)) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ _ _) : _) = do
  funcI <- Property.get $ Sugar.rExpressionPtr func
  isInfix <- Infix.isApplyOfInfixOp funcI
  makeCondHighlightedTextParens isInfix ancestry
getParensInfo (Sugar.ExpressionApply (Sugar.Apply func _)) ancestry = do
  funcI <- Property.get $ Sugar.rExpressionPtr func
  isInfix <- Infix.isInfixFunc funcI
  makeCondHighlightedTextParens isInfix ancestry
getParensInfo (Sugar.ExpressionGetVariable _) (A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ _ _) : _) =
  return Nothing
getParensInfo (Sugar.ExpressionGetVariable var) ancestry = do
  name <- Property.get $ Anchors.variableNameRef var
  makeCondUnhighlightedTextParens (Infix.isInfixName name) ancestry
getParensInfo (Sugar.ExpressionWhere _) ancestry =
  if A.isAncestryRHS ancestry
  then return Nothing
  else liftM (Just . SquareParens) $ makeParensId ancestry
getParensInfo (Sugar.ExpressionFunc _) ancestry@(A.AncestryItemApply _ : _) =
  liftM (Just . TextParens DoHighlightParens) $ makeParensId ancestry
getParensInfo _ _ =
  return Nothing

addTextParens
  :: MonadF m
  => (TWidget t m -> TWidget t m)
  -> (TWidget t m -> TWidget t m)
  -> Widget.Id
  -> WidgetT t m
  -> TWidget t m
addTextParens onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeLabel str $ parensPrefix parenId

squareDraw :: Vector2 Widget.R -> Draw.Image Draw.Any
squareDraw (Vector2 w h) = mconcat
  [ Draw.line (0, 0) (w, 0)
  , Draw.line (w, 0) (w, h)
  , Draw.line (w, h) (0, h)
  , Draw.line (0, h) (0, 0)
  ]

squareFrame :: Anim.AnimId -> Vector2 Widget.R -> Anim.Frame
squareFrame animId size =
  Anim.simpleFrameDownscale animId size . void $ squareDraw size

addSquareParens :: Widget.Id -> WidgetT t m -> WidgetT t m
addSquareParens parensId =
  Widget.atImageWithSize addSquareFrame .
  Widget.translateBy (* ((1 - Config.squareParensScaleFactor) / Config.squareParensScaleFactor / 2)) .
  Widget.atSizeDependentWidgetData (Widget.scaleSizeDependentWidgetData Config.squareParensScaleFactor)
  where
    addSquareFrame size = mappend $ squareFrame (Widget.toAnimId parensId) size

makeParensId :: Monad m => A.ExpressionAncestry m -> Transaction ViewTag m Widget.Id
makeParensId (A.AncestryItemApply (A.ApplyParent role _ _ parentPtr) : _) = do
  parentI <- Property.get parentPtr
  return $
    Widget.joinId (WidgetIds.fromIRef parentI)
    [pack $ show role]
makeParensId (A.AncestryItemLambda (A.LambdaParent _ parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId (A.AncestryItemWhere (A.WhereParent (Sugar.Where _ body) role) : _) = do
  bodyI <- Property.get $ Sugar.rExpressionPtr body
  return $
    Widget.joinId (WidgetIds.fromIRef bodyI)
    [pack $ show role]
makeParensId (A.AncestryItemParamType (A.ParamTypeParent parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId [] =
  return $ Widget.Id ["root parens"]

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor WidgetIds.parenHighlightId Config.parenHighlightColor

addParens
  :: (MonadF m)
  => Widget.Id
  -> Sugar.Expression m
  -> A.ExpressionAncestry m
  -> WidgetT ViewTag m
  -> TWidget ViewTag m
addParens myId sExpr ancestry widget = do
  mInfo <- transaction $ getParensInfo sExpr ancestry
  case mInfo of
    Nothing -> return widget
    Just (SquareParens parensId) -> return $ addSquareParens parensId widget
    Just (TextParens needHighlight parensId) -> do
      mInsideParenId <- subCursor rParenId
      widgetWithParens <- addTextParens id doHighlight parensId widget
      return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
      where
        rParenId = Widget.joinId myId [")"]
        doHighlight =
          case needHighlight of
            DoHighlightParens -> (>>= BWidgets.makeFocusableView rParenId)
            DontHighlightParens -> id
