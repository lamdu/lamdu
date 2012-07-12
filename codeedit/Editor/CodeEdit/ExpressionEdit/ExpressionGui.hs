{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.ExpressionGui
  ( ExpressionGui(..), atEgWidget
  , Maker
  , fromValueWidget
  , hbox, hboxSpaced
  , addType
  ) where

import Control.Arrow (second)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.OTransaction (OTransaction, WidgetT)
import Graphics.UI.Bottle.Widget (R)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.List as List
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

data ExpressionGui m = ExpressionGui
  { egWidget :: WidgetT ViewTag m
  , egAlignment :: R
  }

AtFieldTH.make ''ExpressionGui

type Maker m = Sugar.ExpressionRef m -> OTransaction ViewTag m (ExpressionGui m)

fromValueWidget :: WidgetT ViewTag m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 1

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case Box.boxContent box of
  ((_, x) : _) -> Vector2.snd $ Grid.elementAlign x
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget _alignment) = (0, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.spaceWidget)

addType
  :: Widget.Id
  -> [WidgetT ViewTag m]
  -> WidgetT ViewTag m
  -> ExpressionGui m
addType _ [] widget = fromValueWidget widget
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
