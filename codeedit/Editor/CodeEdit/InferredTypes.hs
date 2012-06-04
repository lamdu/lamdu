module Editor.CodeEdit.InferredTypes(addType) where

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

addType
  :: MonadF m
  => Widget.Id
  -> [Widget (Transaction ViewTag m)]
  -> Widget (Transaction ViewTag m)
  -> Widget (Transaction ViewTag m)
addType _ [] widget = widget
addType exprId typeEdits widget =
  Box.toWidget $ Box.make Box.vertical
  [ Widget.align (Vector2 0.5 0.5) widget
    -- must not be aligned if space is to be used
  , Spacer.makeHorizLineWidget underlineId
  , Widget.align (Vector2 0.5 0.5) typeEdit
  ]
  where
    typeEdit = Widget.scale Config.typeScaleFactor $ BWidgets.vbox typeEdits
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

