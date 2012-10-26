{-# LANGUAGE TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.ExpressionGui.Types (WidgetT, ExpressionGui(..), atEgWidget) where

import Editor.Anchors (ViewTag)
import Graphics.UI.Bottle.Widget (R)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Editor.ITransaction as IT

type WidgetT m = IT.WidgetT ViewTag m

data ExpressionGui m = ExpressionGui
  { egWidget :: WidgetT m
  , egAlignment :: R
  }
AtFieldTH.make ''ExpressionGui
