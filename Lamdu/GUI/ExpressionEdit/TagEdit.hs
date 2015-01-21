{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
  ( makeRecordTag, makeParamTag, diveIntoRecordTag
  ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [ModKey mempty GLFW.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKeys = [ModKey mempty GLFW.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

makeRecordTagInner ::
  MonadA m => Sugar.TagG (Name m) -> Widget.Id -> ExprGuiM m (Widget (T m))
makeRecordTagInner tagG myId = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameEdit (tagG ^. Sugar.tagGName) myId
    & ExprGuiM.withFgColor recordTagColor
    <&> Widget.scale (recordTagScaleFactor <&> realToFrac)

makeRecordTag ::
  MonadA m =>
  NearestHoles -> Sugar.TagG (Name m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeRecordTag nearestHoles tagG myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap [] nearestHoles
  let
    eventMap =
      jumpHolesEventMap <>
      maybe mempty jumpNextEventMap (nearestHoles ^. NearestHoles.next)
  let Config.Name{..} = Config.name config
  ExprGuiM.wrapDelegated fdConfig FocusDelegator.NotDelegating id
    (makeRecordTagInner tagG) myId
    <&> Widget.weakerEvents eventMap
    <&> ExpressionGui.fromValueWidget
  where
    jumpNextEventMap nextHole =
      Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Space]
      (E.Doc ["Navigation", "Jump to next hole"]) $
      return $ WidgetIds.fromEntityId nextHole

-- | Unfocusable tag view (e.g: in apply params)
makeParamTag ::
  MonadA m => Sugar.TagG (Name m) -> AnimId -> ExprGuiM m (ExpressionGui m)
makeParamTag t animId = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameView (t ^. Sugar.tagGName) animId
    & ExprGuiM.widgetEnv
    & ExprGuiM.withFgColor paramTagColor
    <&> Widget.scale (paramTagScaleFactor <&> realToFrac)
    <&> ExpressionGui.fromValueWidget

diveIntoRecordTag :: Widget.Id -> Widget.Id
diveIntoRecordTag = FocusDelegator.delegatingId
