{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView, colorOf) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP)
import Editor.MonadF(MonadF)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Data.VariableRef -> Draw.Color
colorOf (Data.BuiltinRef _) = Config.builtinColor
colorOf (Data.DefinitionRef _) = Config.definitionColor
colorOf (Data.ParameterRef _) = Config.parameterColor

fixName :: String -> String
fixName "" = Config.unnamedStr
fixName x = x

makeView
  :: MonadF m
  => Data.VariableRef -> Widget.Id -> TWidget t m
makeView var myId = do
  name <- liftM fixName . getP $ Anchors.variableNameRef var
  BWidgets.setTextColor (colorOf var) $
    BWidgets.makeFocusableTextView name myId

make
  :: MonadF m
  => Data.VariableRef -> Widget.Id
  -> TWidget ViewTag m
make varRef myId = do
  varRefView <- makeView varRef myId
  let
    jumpToDefinitionEventMap =
      Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
    jumpToDefinition =
      case varRef of
        Data.DefinitionRef defI -> do
          Anchors.newPane defI
          Anchors.jumpTo myId $ WidgetIds.fromIRef defI
        Data.ParameterRef paramI -> Anchors.jumpTo myId $ WidgetIds.fromIRef paramI
        Data.BuiltinRef _builtI -> return myId
  return $ Widget.weakerEvents jumpToDefinitionEventMap varRefView
