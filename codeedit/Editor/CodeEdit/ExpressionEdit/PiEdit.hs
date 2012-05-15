{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PiEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, assignCursor, atTextSizeColor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.Pi m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Pi param resultType) myId =
  assignCursor myId ((WidgetIds.fromGuid . Sugar.guid . Sugar.rActions) resultType) $ do
    (paramNameEdit, paramTypeEdit) <- FuncEdit.makeParamEdit makeExpressionEdit param
    let paramEdit = BWidgets.vbox [paramNameEdit, paramTypeEdit]
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    resultTypeEdit <- makeExpressionEdit resultType
    return $ BWidgets.hboxSpaced [paramEdit, rightArrowLabel, resultTypeEdit]
