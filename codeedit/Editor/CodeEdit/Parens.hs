{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Parens
  ( addTextParens
  , addHighlightedTextParens
  ) where

import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ExprGuiM, WidgetT)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.Config as Config
import qualified Editor.Layers as Layers
import qualified Editor.WidgetEnvT as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

type WidgetMaker m = ExprGuiM m (WidgetT m)

addTextParensI
  :: MonadF m
  => (WidgetMaker m -> WidgetMaker m)
  -> (WidgetMaker m -> WidgetMaker m)
  -> Anim.AnimId
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addTextParensI onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ ExpressionGui.hbox
    [ ExpressionGui.fromValueWidget beforeParen
    , widget
    , ExpressionGui.fromValueWidget afterParen
    ]
  where
    label str =
      ExprGuiM.otransaction . BWidgets.makeLabel str $ parensPrefix parenId

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor Layers.parensHighlightBG WidgetIds.parenHighlightId Config.parenHighlightColor

addTextParens
  :: MonadF m
  => Anim.AnimId
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addTextParens = addTextParensI id id

addHighlightedTextParens
  :: (MonadF m)
  => Widget.Id
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addHighlightedTextParens myId widget = do
  mInsideParenId <- ExprGuiM.otransaction $ OT.subCursor rParenId
  widgetWithParens <- addTextParensI id doHighlight (Widget.toAnimId myId) widget
  return $ maybe id (const (ExpressionGui.atEgWidget highlightExpression)) mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (ExprGuiM.otransaction . BWidgets.makeFocusableView rParenId =<<)
