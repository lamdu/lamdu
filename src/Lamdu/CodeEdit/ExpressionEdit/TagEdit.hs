{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.TagEdit(make, makeView) where

import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

onTagWidget :: Widget (Transaction m) -> ExpressionGui m
onTagWidget = ExpressionGui.fromValueWidget . Widget.scale Config.tagScale . Widget.tint Config.fieldTint

make
  :: MonadA m
  => Sugar.TagG Sugar.Name
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make (Sugar.TagG tag name) =
  ExpressionGui.wrapDelegated fdConfig FocusDelegator.NotDelegating
  (fmap onTagWidget . ExpressionGui.makeNameEdit name tag)

makeView :: MonadA m => Sugar.TagG Sugar.Name -> AnimId -> ExprGuiM m (ExpressionGui m)
makeView (Sugar.TagG _ name) =
  ExprGuiM.widgetEnv . fmap onTagWidget . ExpressionGui.makeNameView name
