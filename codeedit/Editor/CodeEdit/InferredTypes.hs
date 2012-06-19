{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.InferredTypes(addType, mkInferredTypesView) where

import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

-- non-aligned vbox:
vbox :: [Widget f] -> Widget f
vbox = Box.toWidget . Box.make Box.vertical

center :: Widget f -> Widget f
center = Widget.align (Vector2 0.5 0.5)

mkInferredTypesView
  :: Widget.Id
  -> [Widget f]
  -> [Widget f]
mkInferredTypesView _ [] = []
mkInferredTypesView exprId typeEdits =
  [ -- must not be aligned (needs to take over all given space):
    Spacer.makeHorizLineWidget underlineId
  , center typeEdit
  ]
  where
    typeEdit =
      addErrorBackground .
      Widget.scale Config.typeScaleFactor $
      BWidgets.vbox typeEdits
    isError = length typeEdits >= 2
    typeErrorAnimId = Widget.toAnimId exprId ++ ["type error background"]
    addErrorBackground
      | isError = Widget.backgroundColor 15 typeErrorAnimId Config.typeErrorBackgroundColor
      | otherwise = id
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

addType
  :: Widget.Id
  -> [Widget f]
  -> Widget f
  -> Widget f
addType exprId typeEdits widget =
  vbox $
  center widget :
  mkInferredTypesView exprId typeEdits
