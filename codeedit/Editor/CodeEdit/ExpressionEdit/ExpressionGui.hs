{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.ExpressionEdit.ExpressionGui
  ( ExpressionGui(..), atEgWidget
  , Maker
  , fromValueWidget
  , hbox, hboxSpaced
  ) where

import Editor.Anchors(ViewTag)
import Editor.OTransaction(OTransaction, WidgetT)
import Graphics.UI.Bottle.Widget (R)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.List as List
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

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
    f (ExpressionGui widget alignment) = (alignment, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.spaceWidget)
