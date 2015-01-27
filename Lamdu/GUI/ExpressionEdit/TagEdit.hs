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
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
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

makeRecordTagNameEdit ::
  MonadA m => Sugar.TagG (Name m) -> ExprGuiM m (Widget (T m))
makeRecordTagNameEdit tagG = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameEdit (tagG ^. Sugar.tagGName) myId
    & ExprGuiM.withFgColor recordTagColor
    <&> Widget.scale (recordTagScaleFactor <&> realToFrac)
  where
    myId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)

makeRecordTag ::
  MonadA m =>
  NearestHoles -> Sugar.TagG (Name m) ->
  ExprGuiM m (ExpressionGui m)
makeRecordTag nearestHoles tagG = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  jumpHolesEventMap <-
    ExprEventMap.jumpHolesEventMap [] nearestHoles
    & ExprGuiM.widgetEnv
  let
    eventMap =
      jumpHolesEventMap <>
      maybe mempty jumpNextEventMap (nearestHoles ^. NearestHoles.next)
  let Config.Name{..} = Config.name config
  makeRecordTagNameEdit tagG
    <&> Widget.weakerEvents eventMap
    <&> ExpressionGui.fromValueWidget
  where
    jumpNextEventMap nextHole =
      Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Space]
      (E.Doc ["Navigation", "Jump to next hole"]) $
      return $ WidgetIds.fromEntityId nextHole

-- | Unfocusable tag view (e.g: in apply params)
makeParamTag ::
  MonadA m => Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeParamTag t = do
  Config.Name{..} <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.makeNameView (t ^. Sugar.tagGName) animId
    & ExprGuiM.withFgColor paramTagColor
    <&> Widget.scale (paramTagScaleFactor <&> realToFrac)
    <&> ExpressionGui.fromValueWidget
  where
    animId = t ^. Sugar.tagInstance & WidgetIds.fromEntityId & Widget.toAnimId

diveIntoRecordTag :: Widget.Id -> Widget.Id
diveIntoRecordTag = ExpressionGui.diveToNameEdit
