{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Sugar.GetVariable -> Draw.Color
colorOf (Sugar.GetDefinition _) = Config.definitionColor
colorOf (Sugar.GetParameter _) = Config.parameterColor

-- Color should be determined on the outside!
makeView
  :: MonadF m
  => Sugar.GetVariable
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeView var myId = ExprGuiM.withNameFromVarRef var $ \(nameSrc, name) ->
  liftM
  (ExpressionGui.fromValueWidget .
   ExpressionGui.nameSrcTint nameSrc) .
  ExprGuiM.otransaction $
  BWidgets.makeFocusableTextView name myId

make
  :: MonadF m
  => Sugar.GetVariable
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getVar myId = do
  case getVar of
    Sugar.GetParameter guid -> ExprGuiM.markVariablesAsUsed [guid]
    _ -> return ()
  getVarView <-
    ExprGuiM.atEnv (OT.setTextColor (colorOf getVar)) $
    makeView getVar myId
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition"
      jumpToDefinition
    jumpToDefinition =
      case getVar of
        Sugar.GetDefinition defI -> IT.transaction $ do
          Anchors.newPane defI
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.fromIRef defI
        Sugar.GetParameter paramGuid -> IT.transaction $ do
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.fromGuid paramGuid
  return $ ExpressionGui.atEgWidget (Widget.weakerEvents jumpToDefinitionEventMap) getVarView
