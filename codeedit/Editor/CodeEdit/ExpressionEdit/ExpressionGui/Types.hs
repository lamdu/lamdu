{-# LANGUAGE TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.ExpressionGui.Types (WidgetT, ExpressionGui(..), atEgWidget) where

import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Graphics.UI.Bottle.Widget (Widget, R)
import qualified Data.AtFieldTH as AtFieldTH

type WidgetT m = Widget (Transaction ViewTag m)

data ExpressionGui m = ExpressionGui
  { egWidget :: WidgetT m
  , egAlignment :: R
  }
AtFieldTH.make ''ExpressionGui
