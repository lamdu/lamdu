{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.PiEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mappend)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

make ::
  MonadA m => ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam _ param _isDep resultType) =
  ExpressionGui.wrapParenify parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId typeId $ do
    (resultTypeEdit, usedVars) <-
      ExprGuiM.listenUsedVariables $
      ExprGuiM.makeSubexpresion 0 resultType
    let
      paramUsed = paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    ExprGuiM.atEnv (Lens.over WE.envCursor redirectCursor) $ do
      paramTypeEdit <- ExprGuiM.makeSubexpresion 1 $ param ^. Sugar.fpType
      paramEdit <-
        if paramUsed
        then do
          paramNameEdit <- LambdaEdit.makeParamNameEdit name paramGuid paramId
          colonLabel <- ExprGuiM.widgetEnv . BWidgets.makeLabel ":" $ Widget.toAnimId paramId
          return $ ExpressionGui.hbox
            [ ExpressionGui.fromValueWidget paramNameEdit
            , ExpressionGui.fromValueWidget colonLabel
            , paramTypeEdit
            ]
        else return paramTypeEdit
      rightArrowLabel <-
        ExprGuiM.atEnv (WE.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) .
        ExprGuiM.widgetEnv . BWidgets.makeLabel "→" $ Widget.toAnimId myId
      let
        addBg
          | paramUsed =
              ExpressionGui.egWidget %~
              Widget.backgroundColor
              Layers.collapsedExpandedBG
              (mappend (Widget.toAnimId paramId) ["polymorphic bg"])
              Config.collapsedExpandedBGColor
          | otherwise = id
        paramAndArrow =
          addBg $
          ExpressionGui.hboxSpaced
          [paramEdit, ExpressionGui.fromValueWidget rightArrowLabel]
      return $ ExpressionGui.hboxSpaced [paramAndArrow, resultTypeEdit]
  where
    name = param ^. Sugar.fpName
    paramGuid = param ^. Sugar.fpGuid
    paramId = WidgetIds.fromGuid $ param ^. Sugar.fpId
    typeId =
      WidgetIds.fromGuid $ param ^. Sugar.fpType . Sugar.rGuid
