{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.ExpressionEdit.ExpressionGui
  ( ExpressionGui(..), atEgWidget
  , Maker
  , hbox, hboxSpaced
  ) where

import Editor.Anchors(ViewTag)
import Editor.OTransaction(OTransaction, WidgetT)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar

data ExpressionGui m = ExpressionGui
  { egWidget :: WidgetT ViewTag m
  }

AtFieldTH.make ''ExpressionGui

type Maker m = Sugar.ExpressionRef m -> OTransaction ViewTag m (ExpressionGui m)

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox = ExpressionGui . BWidgets.hboxCentered . map egWidget

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = ExpressionGui . BWidgets.hboxCenteredSpaced . map egWidget
