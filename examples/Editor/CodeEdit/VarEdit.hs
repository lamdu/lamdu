{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.VarEdit(make, makeView) where

import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, getP)
import Editor.MonadF(MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

makeView :: MonadF m => Data.VariableRef -> Widget.Id -> TWidget t m
makeView var myId = do
  name <- getP $ Anchors.variableNameRef var
  let
    color =
      case var of
        Data.BuiltinRef _ -> Config.builtinColor
        Data.DefinitionRef _ -> Config.definitionColor
        Data.ParameterRef _ -> Config.parameterColor
  BWidgets.setTextColor color $
    BWidgets.makeFocusableTextView name myId

make ::
  (Functor m, Monad m) =>
  Data.VariableRef
  -> Widget.Id
  -> CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make varRef myId = do
  varRefView <- makeView varRef myId
  let
    jumpToDefinitionEventMap =
      Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
    jumpToDefinition =
      case varRef of
        Data.DefinitionRef defI -> Anchors.newPane defI >> return (WidgetIds.fromIRef defI)
        Data.ParameterRef paramI -> return $ WidgetIds.fromIRef paramI
        Data.BuiltinRef _builtI -> return myId
  return (Widget.weakerEvents jumpToDefinitionEventMap varRefView, myId)
