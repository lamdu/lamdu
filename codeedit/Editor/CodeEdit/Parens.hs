{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Parens
  ( addTextParens
  , addSquareParens
  , addHighlightedTextParens)
where

import Control.Monad (void)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget)
import Editor.WidgetIds (parensPrefix)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.Config as Config
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

addTextParensI
  :: MonadF m
  => (TWidget ViewTag m -> TWidget ViewTag m)
  -> (TWidget ViewTag m -> TWidget ViewTag m)
  -> Anim.AnimId
  -> ExpressionGui m
  -> OTransaction ViewTag m (ExpressionGui m)
addTextParensI onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ ExpressionGui.hbox [ ExpressionGui.fromValueWidget beforeParen, widget, ExpressionGui.fromValueWidget afterParen ]
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

addSquareParens
  :: Monad m
  => Anim.AnimId
  -> ExpressionGui m
  -> OTransaction ViewTag m (ExpressionGui m)
addSquareParens parensId =
  return . ExpressionGui.atEgWidget
  (Widget.atWFrameWithSize addSquareFrame .
   Widget.translateBy (* ((1 - Config.squareParensScaleFactor) / Config.squareParensScaleFactor / 2)) .
   Widget.scale Config.squareParensScaleFactor)
  where
    addSquareFrame size = mappend $ squareFrame parensId size

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor 11 WidgetIds.parenHighlightId Config.parenHighlightColor

addTextParens
  :: MonadF m
  => Anim.AnimId
  -> ExpressionGui m
  -> OTransaction ViewTag m (ExpressionGui m)
addTextParens = addTextParensI id id

addHighlightedTextParens
  :: (MonadF m)
  => Widget.Id
  -> ExpressionGui m
  -> OTransaction ViewTag m (ExpressionGui m)
addHighlightedTextParens myId widget = do
  mInsideParenId <- OT.subCursor rParenId
  widgetWithParens <- addTextParensI id doHighlight (Widget.toAnimId myId) widget
  return $ maybe id (const (ExpressionGui.atEgWidget highlightExpression)) mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (>>= BWidgets.makeFocusableView rParenId)
