{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.GetVarEdit
  ( make, makeUncoloredView, makeView
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetIds as WidgetIds

makeUncoloredView
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeUncoloredView getVar myId =
  fmap ExpressionGui.fromValueWidget $
  ExprGuiM.widgetEnv . BWidgets.makeFocusableView myId =<<
  maybeAppendDefSuffix =<<
  ExpressionGui.nameSrcTint nameSrc <$>
  ExprGuiM.widgetEnv (BWidgets.makeTextView name animId)
  where
    animId = Widget.toAnimId myId
    label = ExprGuiM.widgetEnv . flip BWidgets.makeLabel animId
    maybeAppendDefSuffix w =
      case getVar ^. Sugar.gvVarType of
      Sugar.GetParametersOfDef (_, Sugar.Name _ defName) -> do
        prefixLabel <- label "(of "
        defNameLabel <-
          ExprGuiM.withFgColor Config.definitionColor $ label defName
        suffixLabel <- label ")"
        return $
          BWidgets.hboxCenteredSpaced
          [ w
          , Widget.scale Config.paramDefSuffixScaleFactor $
            Box.hboxCentered [prefixLabel, defNameLabel, suffixLabel]
          ]
      _ -> return w
    Sugar.Name nameSrc name = getVar ^. Sugar.gvName

colorOf :: Sugar.GetVarType m -> Draw.Color
colorOf Sugar.GetDefinition = Config.definitionColor
colorOf Sugar.GetParameter = Config.parameterColor
colorOf Sugar.GetFieldParameter = Config.parameterColor
colorOf Sugar.GetParametersOfDef {} = Config.parameterColor

makeView
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeView getParam =
  (ExprGuiM.withFgColor . colorOf) (getParam ^. Sugar.gvVarType) .
  makeUncoloredView getParam

make
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getVar myId = do
  ExprGuiM.markVariablesAsUsed $
    case getVar ^. Sugar.gvVarType of
    Sugar.GetDefinition -> []
    _ -> [getVar ^. Sugar.gvIdentifier]
  cp <- ExprGuiM.readCodeAnchors
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys
      (E.Doc ["Navigation", "Jump to definition"]) $ do
        DataOps.savePreJumpPosition cp myId
        WidgetIds.fromGuid <$> getVar ^. Sugar.gvJumpTo
  makeView getVar myId &
    Lens.over (Lens.mapped . ExpressionGui.egWidget) (Widget.weakerEvents jumpToDefinitionEventMap)
