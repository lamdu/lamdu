{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.ITransaction as IT
import qualified Editor.WidgetEnvT as WE
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Data.VariableRef -> Draw.Color
colorOf (Data.DefinitionRef _) = Config.definitionColor
colorOf (Data.ParameterRef _) = Config.parameterColor

-- Color should be determined on the outside!
makeView
  :: MonadF m
  => Data.VariableRef
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeView var myId = ExprGuiM.withNameFromVarRef var $ \(nameSrc, name) ->
  liftM
  (ExpressionGui.fromValueWidget .
   ExpressionGui.nameSrcTint nameSrc) .
  ExprGuiM.widgetEnv $
  BWidgets.makeFocusableTextView name myId

make
  :: MonadF m
  => Data.VariableRef
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getVar myId = do
  case getVar of
    Data.ParameterRef guid -> ExprGuiM.markVariablesAsUsed [guid]
    _ -> return ()
  getVarView <-
    ExprGuiM.atEnv (WE.setTextColor (colorOf getVar)) $
    makeView getVar myId
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition"
      jumpToDefinition
    jumpToDefinition =
      case getVar of
        Data.DefinitionRef defI -> IT.transaction $ do
          Anchors.newPane defI
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.fromIRef defI
        Data.ParameterRef paramGuid -> IT.transaction $ do
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.fromGuid paramGuid
  return $ ExpressionGui.atEgWidget (Widget.weakerEvents jumpToDefinitionEventMap) getVarView
