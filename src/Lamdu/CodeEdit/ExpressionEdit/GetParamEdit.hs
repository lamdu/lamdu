{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.GetParamEdit(make) where

import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadA m
  => Sugar.GetParam
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getParam myId = do
  (_, name) <- ExprGuiM.transaction . ExprGuiM.getGuidName $ Sugar.gpTag getParam
  cp <- ExprGuiM.readCodeAnchors
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys
      (E.Doc ["Navigation", "Jump to definition"]) $ do
        DataOps.savePreJumpPosition cp myId
        return . WidgetIds.fromGuid $ Sugar.gpJumpTo getParam
  fmap (ExpressionGui.fromValueWidget . Widget.weakerEvents jumpToDefinitionEventMap) .
    ExprGuiM.withFgColor Config.parameterColor .
    ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView name myId
