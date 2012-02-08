{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.VarView(make) where

import Editor.CTransaction (TWidget, getP)
import Editor.MonadF(MonadF)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

make :: MonadF m => Data.VariableRef -> Widget.Id -> TWidget t m
make var myId = do
  name <- getP $ Anchors.variableNameRef var
  let
    color =
      case var of
        Data.BuiltinRef _ -> Config.builtinColor
        Data.DefinitionRef _ -> Config.definitionColor
        Data.ParameterRef _ -> Config.parameterColor
  BWidgets.setTextColor color $
    BWidgets.makeFocusableTextView name myId
