{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag
    , makeParamTag
    , diveToRecordTag, diveToCaseTag
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeTagNameEdit ::
    Monad m =>
    Widget.EventMap (T m Widget.EventResult) -> Draw.Color ->
    Sugar.Tag (Name m) -> ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeTagNameEdit jumpNextEventMap tagColor tag =
    ExpressionGui.makeNameEdit (Align.tValue %~ E.weakerEvents jumpNextEventMap)
    (tag ^. Sugar.tagName) myId
    & Reader.local (TextView.color .~ tagColor)
    <&> Align.tValue . E.eventMap %~ E.filterChars (/= ',')
    where
        myId = WidgetIds.fromEntityId (tag ^. Sugar.tagInfo . Sugar.tagInstance)

makeTagH ::
    Monad m =>
    Draw.Color -> NearestHoles -> Sugar.Tag (Name m) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeTagH tagColor nearestHoles tag =
    do
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        let keys = Config.holePickAndMoveToNextHoleKeys (Config.hole config)
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & maybe mempty
                  (Widget.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   return . WidgetIds.fromEntityId)
        let Theme.Name{..} = Theme.name theme
        makeTagNameEdit jumpNextEventMap tagColor tag
            <&> Align.tValue %~ E.weakerEvents jumpHolesEventMap

makeRecordTag ::
    Monad m => NearestHoles -> Sugar.Tag (Name m) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeRecordTag nearestHoles tag =
    do
        Theme.Name{..} <- Theme.name <$> Lens.view Theme.theme
        makeTagH recordTagColor nearestHoles tag

makeCaseTag ::
    Monad m => NearestHoles -> Sugar.Tag (Name m) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeCaseTag nearestHoles tag =
    do
        Theme.Name{..} <- Theme.name <$> Lens.view Theme.theme
        makeTagH caseTagColor nearestHoles tag

-- | Unfocusable tag view (e.g: in apply params)
makeParamTag :: Monad m => Sugar.Tag (Name m) -> ExprGuiM m (WithTextPos View)
makeParamTag t =
    do
        Theme.Name{..} <- Theme.name <$> Lens.view Theme.theme
        ExpressionGui.makeNameView (t ^. Sugar.tagName) animId
            & Reader.local (TextView.color .~ paramTagColor)
    where
        animId = t ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId & Widget.toAnimId

diveToRecordTag :: Widget.Id -> Widget.Id
diveToRecordTag = WidgetIds.nameEditOf

diveToCaseTag :: Widget.Id -> Widget.Id
diveToCaseTag = WidgetIds.nameEditOf
