{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.InferredTypes(addType) where

import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

addType
  :: Widget.Id
  -> [Widget f]
  -> Widget f
  -> Widget f
addType _ [] widget = widget
addType exprId typeEdits widget =
  BWidgets.vboxCentered $
  [ widget
  , Spacer.makeHorizLine underlineId (Vector2 underLineWidth 1)
  , typeEdit
  ]
  where
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
