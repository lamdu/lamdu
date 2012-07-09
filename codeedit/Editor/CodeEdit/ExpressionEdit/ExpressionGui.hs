module Editor.CodeEdit.ExpressionEdit.ExpressionGui(Maker) where

import Editor.Anchors(ViewTag)
import Editor.OTransaction(TWidget)
import qualified Editor.CodeEdit.Sugar as Sugar

type Maker m = Sugar.ExpressionRef m -> TWidget ViewTag m
