{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.Hover (addBackground)

import           Lamdu.Prelude

textEditNoEmpty :: TextEdit.EmptyStrings
textEditNoEmpty = TextEdit.EmptyStrings "  " "  "

makeSearchTermPropEdit ::
    (MonadReader env m, Widget.HasCursor env, TextEdit.HasStyle env, Monad f) =>
    WidgetIds -> Property f Text ->
    m (WithTextPos (Widget (f Widget.EventResult)))
makeSearchTermPropEdit widgetIds searchTermProp =
    TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? hidOpenSearchTerm widgetIds
    <&> Align.tValue . Widget.events %~ \(newSearchTerm, eventRes) ->
        do
            when (newSearchTerm /= searchTerm) $
                Property.set searchTermProp newSearchTerm
            eventRes
                -- When first letter is typed in search term, jump to the
                -- results, which will go to first result:
                & ( if Text.null searchTerm && (not . Text.null) newSearchTerm
                    then
                        Widget.eCursor .~
                        Monoid.Last (Just (hidResultsPrefix widgetIds))
                    else id
                  )
                & return
    where
        searchTerm = Property.value searchTermProp

make :: Monad m => HoleInfo m -> ExprGuiM m (WithTextPos (Widget (Transaction m Widget.EventResult)))
make holeInfo =
    do
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        let holeConfig = Config.hole config
        let holeTheme = Theme.hole theme
        textCursor <- TextEdit.getCursor ?? searchTerm ?? hidOpenSearchTerm widgetIds
        makeSearchTermPropEdit widgetIds (HoleInfo.hiSearchTermProperty holeInfo)
            <&> Align.tValue . E.eventMap
                %~ EventMap.disallowCharsFromSearchTerm holeConfig holeInfo textCursor
            <&> addBackground (Widget.toAnimId (hidOpenSearchTerm widgetIds))
                (Theme.holeSearchTermBGColor holeTheme)
    where
        widgetIds = hiIds holeInfo
        searchTerm = HoleInfo.hiSearchTerm holeInfo
