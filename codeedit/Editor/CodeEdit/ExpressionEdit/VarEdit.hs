{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView, colorOf) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Data.VariableRef -> Draw.Color
colorOf (Data.DefinitionRef _) = Config.definitionColor
colorOf (Data.ParameterRef _) = Config.parameterColor

makeView
  :: MonadF m
  => Data.VariableRef
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
makeView var myId = do
  name <-
    OT.transaction . BWidgets.getDisplayNameOf $ Data.variableRefGuid var
  liftM ExpressionGui .
    BWidgets.setTextColor (colorOf var) $
    BWidgets.makeFocusableTextView name myId

make
  :: MonadF m
  => Data.VariableRef
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
make varRef myId = do
  OT.markVariablesAsUsed [varRef]
  varRefView <- makeView varRef myId
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
    jumpToDefinition =
      case varRef of
        Data.DefinitionRef defI -> IT.transaction $ do
          Anchors.newPane defI
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.fromIRef defI
        Data.ParameterRef paramGuid -> IT.transaction $ do
          Anchors.savePreJumpPosition myId
          return $ WidgetIds.paramId paramGuid
  return $ ExpressionGui.atEgWidget (Widget.weakerEvents jumpToDefinitionEventMap) varRefView
