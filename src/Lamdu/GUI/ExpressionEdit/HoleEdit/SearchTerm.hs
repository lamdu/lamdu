{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (HasConfig)
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds

import           Lamdu.Prelude

textEditNoEmpty :: TextEdit.EmptyStrings
textEditNoEmpty = TextEdit.EmptyStrings "  " "  "

make ::
    ( Monad m, MonadReader env f, HasTheme env, Widget.HasCursor env
    , HasConfig env, TextEdit.HasStyle env
    ) => HoleInfo m -> f (WithTextPos (Widget (Transaction m Widget.EventResult)))
make holeInfo =
    do
        theme <- Lens.view Theme.theme
        let holeTheme = Theme.hole theme
        textCursor <- TextEdit.getCursor ?? searchTerm ?? hidOpenSearchTerm widgetIds
        isActive <- WidgetIds.isActive widgetIds
        let bgColor
                | isActive = Theme.holeActiveSearchTermBGColor
                | otherwise = Theme.holeSearchTermBGColor
        disallowChars <- EventMap.disallowCharsFromSearchTerm
        TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? hidOpenSearchTerm widgetIds
            <&> Align.tValue . Widget.events %~ onEvents
            <&> Align.tValue . E.eventMap %~ disallowChars holeInfo textCursor
            <&> Draw.backgroundColor bgAnimId (bgColor holeTheme)
    where
        onEvents (newSearchTerm, eventRes) =
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
        bgAnimId =
            Widget.toAnimId (hidOpenSearchTerm widgetIds) <> ["hover background"]
        widgetIds = hiIds holeInfo
        searchTerm = Property.value searchTermProp
        searchTermProp = HoleInfo.hiSearchTermProperty holeInfo
