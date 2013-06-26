{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.TagEdit(make, makeView) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Config (Config)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar.Types as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.WidgetEnvT as WE

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

onTagWidget :: Config -> Widget (Transaction m) -> ExpressionGui m
onTagWidget config =
  ExpressionGui.fromValueWidget .
  Widget.scale (realToFrac <$> Config.tagScaleFactor config) .
  Widget.tint (Config.fieldTint config)

make ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Sugar.TagG Sugar.Name ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make pl (Sugar.TagG tag name) myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.wrapDelegated pl fdConfig FocusDelegator.NotDelegating
    (fmap (onTagWidget config) . ExpressionGui.makeNameEdit name tag) myId

makeView ::
  MonadA m => Sugar.TagG Sugar.Name -> AnimId -> ExprGuiM m (ExpressionGui m)
makeView (Sugar.TagG _ name) animId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv . fmap (onTagWidget config) $
    ExpressionGui.makeNameView name animId
