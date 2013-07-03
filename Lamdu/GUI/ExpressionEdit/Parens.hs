{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.Parens
  ( addTextParens
  , addHighlightedTextParens
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.WidgetIds (parensPrefix)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds

type WidgetMaker m = ExprGuiM m (WidgetT m)

addTextParensI
  :: MonadA m
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
      ExprGuiM.widgetEnv . BWidgets.makeLabel str $ parensPrefix parenId

highlightExpression :: Config -> Widget.Widget f -> Widget.Widget f
highlightExpression config =
  Widget.backgroundColor (Config.layerParensHighlightBG (Config.layers config))
  WidgetIds.parenHighlightId $
  Config.parenHighlightColor config

addTextParens
  :: MonadA m
  => Anim.AnimId
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addTextParens = addTextParensI id id

addHighlightedTextParens
  :: (MonadA m)
  => Widget.Id
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addHighlightedTextParens myId widget = do
  mInsideParenId <- ExprGuiM.widgetEnv $ WE.subCursor rParenId
  widgetWithParens <- addTextParensI id doHighlight (Widget.toAnimId myId) widget
  config <- ExprGuiM.widgetEnv WE.readConfig
  return $
    widgetWithParens
    & case mInsideParenId of
      Nothing -> id
      Just _ -> ExpressionGui.egWidget %~ highlightExpression config
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (ExprGuiM.widgetEnv . BWidgets.makeFocusableView rParenId =<<)
