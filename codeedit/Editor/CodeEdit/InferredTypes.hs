{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.InferredTypes(addType) where

import Control.Arrow (second)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Editor.OTransaction (WidgetT)
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

addType
  :: Widget.Id
  -> [WidgetT ViewTag m]
  -> WidgetT ViewTag m
  -> ExpressionGui m
addType _ [] widget = ExpressionGui.fromValueWidget widget
addType exprId typeEdits widget =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!") (Vector2.snd . Grid.elementAlign) .
      lookup True $ Box.boxContent box
    box = Box.makeKeyed Box.vertical . (map . second) ((,) 0.5) $
      [ (False, widget)
      , (True,  Spacer.makeHorizLine underlineId (Vector2 underLineWidth 1))
      , (False, typeEdit)
      ]
    width = Vector2.fst . Widget.wSize
    underLineWidth = max (width widget) (width typeEdit)
    typeEdit =
      addTint . addBackground .
      Widget.scale Config.typeScaleFactor $
      BWidgets.vboxCentered typeEdits
    isError = length typeEdits >= 2
    typeErrorAnimId = Widget.toAnimId exprId ++ ["type error background"]
    addTint = (Widget.atWFrame . Anim.onImages . Draw.tint) Config.inferredTypeTint
    addBackground
      | isError =
        Widget.backgroundColor 15 typeErrorAnimId
        Config.inferredTypeErrorBGColor
      | otherwise = id
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId
