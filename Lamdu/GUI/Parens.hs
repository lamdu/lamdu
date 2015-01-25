{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.Parens
  ( addHighlightedTextParens
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Lamdu.GUI.WidgetEnvT as WE
import           Lamdu.GUI.WidgetIds (parensPrefix)
import qualified Lamdu.GUI.WidgetIds as WidgetIds

addTextParensI ::
  MonadA m =>
  (WidgetEnvT m (Widget f) -> WidgetEnvT m (Widget f)) ->
  Anim.AnimId -> Layout f -> WidgetEnvT m (Layout f)
addTextParensI onRParen parenId widget = do
  beforeParen <- label "("
  afterParen <- label ")" & onRParen
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
  (MonadA m, Applicative f) => Widget.Id -> Layout f -> WidgetEnvT m (Layout f)
addHighlightedTextParens myId widget = do
  mInsideParenId <- WE.subCursor rParenId
  widgetWithParens <-
    widget
    & addTextParensI (BWidgets.makeFocusableView rParenId =<<) animId
  config <- WE.readConfig
  return $
    widgetWithParens
    & case mInsideParenId of
      Nothing -> id
      Just _ -> Layout.alignedWidget . _2 %~ highlightExpression config
  where
    rParenId = Widget.joinId myId [")"]
    animId = Widget.toAnimId myId
