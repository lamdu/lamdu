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
import Editor.OTransaction (TWidget, WidgetT)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.OTransaction as OT
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

addTextParensI
  :: MonadF m
  => (TWidget t m -> TWidget t m)
  -> (TWidget t m -> TWidget t m)
  -> Anim.AnimId
  -> WidgetT t m
  -> TWidget t m
addTextParensI onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ BWidgets.hboxCentered [ beforeParen, widget, afterParen ]
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

addSquareParens :: Monad m => Anim.AnimId -> WidgetT t m -> TWidget t m
addSquareParens parensId =
  return .
  Widget.atFrameWithSize addSquareFrame .
  Widget.translateBy (* ((1 - Config.squareParensScaleFactor) / Config.squareParensScaleFactor / 2)) .
  Widget.atSizeDependentWidgetData (Widget.scaleSizeDependentWidgetData Config.squareParensScaleFactor)
  where
    addSquareFrame size = mappend $ squareFrame parensId size

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor 11 WidgetIds.parenHighlightId Config.parenHighlightColor

addTextParens
  :: MonadF m
  => Anim.AnimId
  -> WidgetT t m
  -> TWidget t m
addTextParens = addTextParensI id id

addHighlightedTextParens
  :: (MonadF m)
  => Widget.Id
  -> WidgetT ViewTag m
  -> TWidget ViewTag m
addHighlightedTextParens myId widget = do
  mInsideParenId <- OT.subCursor rParenId
  widgetWithParens <- addTextParensI id doHighlight (Widget.toAnimId myId) widget
  return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (>>= BWidgets.makeFocusableView rParenId)
