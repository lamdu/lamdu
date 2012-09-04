{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PiEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.HasParens
  -> Sugar.Pi m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit hasParens (Sugar.Pi param resultType) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  VarAccess.assignCursor myId typeId $ do
    -- TODO: We pollute the resultTypeEdit with our generated name
    -- (which it will skip) even if we end up non-dependent and don't
    -- have a name
    (name, (resultTypeEdit, usedVars)) <-
      VarAccess.withName paramGuid $ \name ->
      liftM ((,) name) . VarAccess.usedVariables $
      FuncEdit.makeResultEdit makeExpressionEdit [paramId] resultType
    let
      paramUsed = paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    VarAccess.atEnv (OT.atEnvCursor redirectCursor) $ do
      paramTypeEdit <- makeExpressionEdit $ Sugar.fpType param
      paramEdit <-
        if paramUsed
        then do
          paramNameEdit <- FuncEdit.makeParamNameEdit name paramGuid
          colonLabel <- VarAccess.otransaction . BWidgets.makeLabel ":" $ Widget.toAnimId paramId
          return $ ExpressionGui.hbox
            [ ExpressionGui.fromValueWidget paramNameEdit
            , ExpressionGui.fromValueWidget colonLabel
            , paramTypeEdit
            ]
        else return paramTypeEdit
      rightArrowLabel <-
        VarAccess.atEnv (OT.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) .
        VarAccess.otransaction . BWidgets.makeLabel "→" $ Widget.toAnimId myId
      return $ ExpressionGui.hboxSpaced
        [paramEdit, ExpressionGui.fromValueWidget rightArrowLabel, resultTypeEdit]
  where
    paramGuid = Sugar.fpGuid param
    paramId = WidgetIds.fromGuid paramGuid
    typeId =
      WidgetIds.fromGuid . Sugar.rGuid . Sugar.fpType $
      param
