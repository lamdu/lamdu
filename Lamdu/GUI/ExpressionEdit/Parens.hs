{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.Parens
  ( addTextParens
  , addHighlightedTextParens
  ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import           Lamdu.GUI.WidgetIds (parensPrefix)
import qualified Lamdu.GUI.WidgetIds as WidgetIds

type T = Transaction
type WidgetMaker m = ExprGuiM m (Widget (T m))

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
    label str = ExprGuiM.makeLabel str $ parensPrefix parenId

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
