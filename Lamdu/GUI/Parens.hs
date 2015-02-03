{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.Parens
  ( addHighlightedTextParens
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.GUI.WidgetIds (parensPrefix)
import qualified Lamdu.GUI.WidgetIds as WidgetIds

addTextParensI ::
  (MonadA m, Applicative f) =>
  AnimId -> Widget.Id -> Layout f -> WidgetEnvT m (Layout f)
addTextParensI parenId rParenId widget = do
  beforeParen <- label "("
  afterParen <- label ")" >>= BWidgets.makeFocusableView rParenId
  Layout.hbox 0.5
    [ beforeParen & Layout.fromCenteredWidget
    , widget
    , afterParen & Layout.fromCenteredWidget
    ] & return
  where
    label str = BWidgets.makeLabel str $ parensPrefix parenId

highlightExpression :: Config -> Widget.Widget f -> Widget.Widget f
highlightExpression config =
  Widget.backgroundColor (Config.layerParensHighlightBG (Config.layers config))
  WidgetIds.parenHighlightId $
  Config.parenHighlightColor config

addHighlightedTextParens ::
  (MonadA m, Applicative f) => Widget.Id -> Layout f -> ExprGuiM m (Layout f)
addHighlightedTextParens myId widget = do
  mInsideParenId <- WE.subCursor rParenId & ExprGuiM.widgetEnv
  widgetWithParens <- addTextParensI animId rParenId widget & ExprGuiM.widgetEnv
  config <- ExprGuiM.readConfig
  return $
    widgetWithParens
    & case mInsideParenId of
      Nothing -> id
      Just _ -> Layout.alignedWidget . _2 %~ highlightExpression config
  where
    rParenId = Widget.joinId myId [")"]
    animId = Widget.toAnimId myId
