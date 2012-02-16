{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.VarEdit(make, makeView, colorOf) where

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
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

colorOf :: Data.VariableRef -> Draw.Color
colorOf (Data.BuiltinRef _) = Config.builtinColor
colorOf (Data.DefinitionRef _) = Config.definitionColor
colorOf (Data.ParameterRef _) = Config.parameterColor

makeView :: MonadF m => Data.VariableRef -> Widget.Id -> TWidget t m
makeView var myId = do
  name <- getP $ Anchors.variableNameRef var
  BWidgets.setTextColor (colorOf var) $
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
        Data.DefinitionRef defI -> do
          Anchors.newPane defI
          Anchors.jumpTo myId $ WidgetIds.fromIRef defI
        Data.ParameterRef paramI -> Anchors.jumpTo myId $ WidgetIds.fromIRef paramI
        Data.BuiltinRef _builtI -> return myId
  return (Widget.weakerEvents jumpToDefinitionEventMap varRefView, myId)
