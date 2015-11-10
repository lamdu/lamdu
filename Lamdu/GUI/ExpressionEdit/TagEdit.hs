{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag
    , makeParamTag
    , diveToRecordTag, diveToCaseTag
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

makeTagNameEdit ::
    MonadA m => Widget.EventHandlers (T m) -> Draw.Color ->
    Sugar.TagG (Name m) -> ExprGuiM m (Widget (T m))
makeTagNameEdit jumpNextEventMap tagColor tagG =
    ExpressionGui.makeNameEditWith (Widget.weakerEvents jumpNextEventMap)
    (tagG ^. Sugar.tagGName) myId
    & ExprGuiM.withFgColor tagColor
    where
        myId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)

makeTagH ::
    MonadA m =>
    Draw.Color -> NearestHoles -> Sugar.TagG (Name m) ->
    ExprGuiM m (ExpressionGui m)
makeTagH tagColor nearestHoles tagG =
    do
        config <- ExprGuiM.readConfig
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        let keys = Config.holePickAndMoveToNextHoleKeys (Config.hole config)
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & maybe mempty
                  (Widget.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   return . WidgetIds.fromEntityId)
        let Config.Name{..} = Config.name config
        makeTagNameEdit jumpNextEventMap tagColor tagG
            <&> Widget.weakerEvents jumpHolesEventMap
            <&> ExpressionGui.fromValueWidget

makeRecordTag ::
    MonadA m => NearestHoles -> Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeRecordTag nearestHoles tagG =
    do
        Config.Name{..} <- Config.name <$> ExprGuiM.readConfig
        makeTagH recordTagColor nearestHoles tagG

makeCaseTag ::
    MonadA m => NearestHoles -> Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeCaseTag nearestHoles tagG =
    do
        Config.Name{..} <- Config.name <$> ExprGuiM.readConfig
        makeTagH caseTagColor nearestHoles tagG

-- | Unfocusable tag view (e.g: in apply params)
makeParamTag ::
    MonadA m => Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeParamTag t =
    do
        Config.Name{..} <- Config.name <$> ExprGuiM.readConfig
        ExpressionGui.makeNameView (t ^. Sugar.tagGName) animId
            & ExprGuiM.withFgColor paramTagColor
            <&> ExpressionGui.fromValueWidget
    where
        animId = t ^. Sugar.tagInstance & WidgetIds.fromEntityId & Widget.toAnimId

diveToRecordTag :: Widget.Id -> Widget.Id
diveToRecordTag = WidgetIds.nameEditOf

diveToCaseTag :: Widget.Id -> Widget.Id
diveToCaseTag = WidgetIds.nameEditOf
