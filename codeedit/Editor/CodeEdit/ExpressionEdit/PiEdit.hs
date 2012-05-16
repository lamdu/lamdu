{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PiEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, assignCursor, atCursor, atTextSizeColor, usedVariables)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Data.Store.IRef as IRef
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
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
  assignCursor myId typeId $ do
    (resultTypeEdit, usedVars) <- usedVariables $ makeExpressionEdit resultType
    let
      paramGuid = Sugar.guid $ Sugar.fpActions param
      paramUsed = any ((== paramGuid) . Data.onVariableIRef IRef.guid) usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise = case Widget.subId (WidgetIds.paramId paramGuid) cursor of
          Nothing -> cursor
          Just _ -> typeId
    atCursor redirectCursor $ do
      (paramNameEdit, paramTypeEdit) <- FuncEdit.makeParamEdit makeExpressionEdit param
      rightArrowLabel <-
        atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor .
        BWidgets.makeFocusableTextView "→" $ Widget.joinId myId ["arrow"]
      let
        paramEdit
          | paramUsed = BWidgets.vbox [Widget.scale Config.piValueScaleFactor paramNameEdit, paramTypeEdit]
          | otherwise = paramTypeEdit
      return $ BWidgets.hboxSpaced [paramEdit, rightArrowLabel, resultTypeEdit]
  where
    typeId = (WidgetIds.fromGuid . Sugar.guid . Sugar.rActions . Sugar.fpType) param
