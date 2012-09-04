{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView, colorOf) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Sugar.GetVariable -> Draw.Color
colorOf (Sugar.GetDefinition _) = Config.definitionColor
colorOf (Sugar.GetParameter _) = Config.parameterColor

makeView
  :: MonadF m
  => Sugar.GetVariable
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
makeView var myId = VarAccess.withName (Sugar.gvGuid var) $ \(nameSrc, name) ->
  liftM
  (ExpressionGui.fromValueWidget .
   BWidgets.nameSrcTint nameSrc) .
  VarAccess.atEnv (BWidgets.setTextColor (colorOf var)) .
  VarAccess.otransaction $
  BWidgets.makeFocusableTextView name myId

make
  :: MonadF m
  => Sugar.GetVariable
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make getVar myId = do
  case getVar of
    Sugar.GetParameter guid -> VarAccess.markVariablesAsUsed [guid]
    _ -> return ()
  getVarView <- makeView getVar myId
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
