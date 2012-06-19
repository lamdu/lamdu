{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PiEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.OTransaction (TWidget)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.OTransaction as OT
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.Pi m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Pi param resultType) myId =
  OT.assignCursor myId typeId $ do
    (resultTypeEdit, usedVars) <-
      OT.usedVariables $ makeExpressionEdit resultType
    let
      paramGuid = Sugar.guid $ Sugar.fpActions param
      paramUsed = any ((== paramGuid) . Data.variableRefGuid) usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId (WidgetIds.paramId paramGuid) cursor of
          Nothing -> cursor
          Just _ -> typeId
    OT.atCursor redirectCursor $ do
      (paramNameEdit, paramTypeEdit) <-
        FuncEdit.makeParamEdit makeExpressionEdit param
      rightArrowLabel <-
        OT.atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor .
        BWidgets.makeLabel "→" $ Widget.toAnimId myId
      let
        paramEdit
          | paramUsed = BWidgets.vbox [paramNameEdit, paramTypeEdit]
          | otherwise = paramTypeEdit
      return $
        BWidgets.hboxSpaced [paramEdit, rightArrowLabel, resultTypeEdit]
  where
    typeId =
      WidgetIds.fromGuid . Sugar.guid . Sugar.rActions . Sugar.fpType $
      param
