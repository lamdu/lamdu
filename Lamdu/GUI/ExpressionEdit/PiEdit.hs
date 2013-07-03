{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.PiEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mappend)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam _ param _isDep resultType) pl =
  ExpressionGui.wrapParenify pl parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId typeId $ do
    (resultTypeEdit, usedVars) <-
      ExprGuiM.listenUsedVariables $
      ExprGuiM.makeSubexpression 0 resultType
    let
      paramUsed = paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    ExprGuiM.localEnv (WE.envCursor %~ redirectCursor) $ do
      paramTypeEdit <- ExprGuiM.makeSubexpression 1 $ param ^. Sugar.fpType
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
      config <- ExprGuiM.widgetEnv WE.readConfig
      rightArrowLabel <-
        ExprGuiM.localEnv
        (WE.setTextSizeColor
         (Config.rightArrowTextSize config)
         (Config.rightArrowColor config)) .
        ExprGuiM.widgetEnv . BWidgets.makeLabel "→" $ Widget.toAnimId myId
      let
        addBg
          | paramUsed =
              ExpressionGui.egWidget %~
              Widget.backgroundColor
              (Config.layerCollapsedExpandedBG (Config.layers config))
              (mappend (Widget.toAnimId paramId) ["polymorphic bg"])
              (Config.collapsedExpandedBGColor config)
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
      WidgetIds.fromGuid $ param ^. Sugar.fpType . Sugar.rPayload . Sugar.plGuid
