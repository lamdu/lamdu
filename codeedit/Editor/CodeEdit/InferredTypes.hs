{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.InferredTypes(addType, mkInferredTypesView) where

import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.MonadF (MonadF)
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

mkInferredTypesView
  :: Widget.Id
  -> [Widget (Transaction ViewTag m)]
  -> Widget (Transaction ViewTag m)
mkInferredTypesView _ [] = BWidgets.empty
mkInferredTypesView exprId typeEdits =
  vbox
  [ -- must not be aligned (needs to take over all given space):
    Spacer.makeHorizLineWidget underlineId
  , Widget.align (Vector2 0.5 0.5) typeEdit
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
  :: MonadF m
  => Widget.Id
  -> [Widget (Transaction ViewTag m)]
  -> Widget (Transaction ViewTag m)
  -> Widget (Transaction ViewTag m)
addType exprId typeEdits widget =
  BWidgets.vbox
  [ widget
  , mkInferredTypesView exprId typeEdits
  ]
