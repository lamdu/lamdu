{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.PiEdit(make) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Monoid (mappend)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

make
  :: MonadA m
  => Sugar.HasParens
  -> Sugar.Pi m (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Sugar.Pi param resultType) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId typeId $ do
    -- We allocate a name in the resultTypeEdit context even if we end
    -- up non-dependent and don't have a name. This is the only way to
    -- do it (until 2-pass gui gen), but it is also desirable: when
    -- holes spring up, we don't get all the names shuffled
    -- confusingly.
    (name, (resultTypeEdit, usedVars)) <-
      ExprGuiM.withParamName paramGuid $ \name ->
      fmap ((,) name) . ExprGuiM.usedVariables $
      FuncEdit.makeResultEdit [paramId] resultType
    let
      paramUsed = paramGuid `elem` usedVars
      redirectCursor cursor
        | paramUsed = cursor
        | otherwise =
          case Widget.subId paramId cursor of
          Nothing -> cursor
          Just _ -> typeId
    ExprGuiM.atEnv (Lens.over WE.envCursor redirectCursor) $ do
      paramTypeEdit <- ExprGuiM.makeSubexpresion $ param ^. Sugar.fpType
      paramEdit <-
        if paramUsed
        then do
          paramNameEdit <- FuncEdit.makeParamNameEdit name paramGuid
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
              Lens.over ExpressionGui.egWidget $
              Widget.backgroundColor
              Layers.polymorphicExpandedBG
              (mappend (Widget.toAnimId paramId) ["polymorphic bg"])
              Config.polymorphicExpandedBGColor
          | otherwise = id
        paramAndArrow =
          addBg $
          ExpressionGui.hboxSpaced
          [paramEdit, ExpressionGui.fromValueWidget rightArrowLabel]
      return $ ExpressionGui.hboxSpaced [paramAndArrow, resultTypeEdit]
  where
    paramGuid = param ^. Sugar.fpGuid
    paramId = WidgetIds.fromGuid paramGuid
    typeId =
      WidgetIds.fromGuid $ param ^. Sugar.fpType . Sugar.rGuid
