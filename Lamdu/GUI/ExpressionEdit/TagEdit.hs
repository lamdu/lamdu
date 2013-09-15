{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.TagEdit(make, makeView) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef
import qualified Lamdu.Sugar.Types as Sugar

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

onTagWidget :: Config -> Widget (Transaction m) -> ExpressionGui m
onTagWidget config =
  ExpressionGui.fromValueWidget .
  Widget.scale (realToFrac <$> Config.tagScaleFactor config) .
  Widget.tint (Config.fieldTint config)

make ::
  MonadA m =>
  Sugar.TagG Sugar.Name ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.TagG tag name) pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    nextHoleEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor
       (Config.pickAndMoveToNextHoleKeys config)
       (E.Doc ["Navigation", "Jump to next hole"]) .
       return . WidgetIdIRef.fromGuid) $
      pl ^. Sugar.plData . ExprGuiM.plHoleGuids . ExprGuiM.hgMNextHole
  ExpressionGui.stdWrapDelegated pl fdConfig FocusDelegator.NotDelegating
    ( fmap (onTagWidget config . Widget.weakerEvents nextHoleEventMap)
    . ExpressionGui.makeNameEdit name tag
    ) myId

makeView ::
  MonadA m => Sugar.TagG Sugar.Name -> AnimId -> ExprGuiM m (ExpressionGui m)
makeView (Sugar.TagG _ name) animId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv . fmap (onTagWidget config) $
    ExpressionGui.makeNameView name animId
