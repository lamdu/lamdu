{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (HasConfig)
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (readSearchTerm)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

textEditNoEmpty :: TextEdit.EmptyStrings
textEditNoEmpty = TextEdit.EmptyStrings "  " "  "

make ::
    ( Monad m, MonadReader env f, HasTheme env
    , HasConfig env, TextEdit.HasStyle env
    , GuiState.HasState env
    ) =>
    WidgetIds -> Sugar.HoleKind g e0 e1 ->
    f (WithTextPos (Widget (T m GuiState.Update)))
make widgetIds holeKind =
    do
        searchTerm <- readSearchTerm widgetIds
        theme <- Lens.view Theme.theme
        let holeTheme = Theme.hole theme
        isActive <- HoleWidgetIds.isActive widgetIds
        let bgColor
                | isActive = Theme.holeActiveSearchTermBGColor
                | otherwise = Theme.holeSearchTermBGColor
        disallowChars <- EventMap.disallowCharsFromSearchTerm
        let
            onEvents (newSearchTerm, eventRes)
                | newSearchTerm == searchTerm = eventRes
                | otherwise =
                    eventRes
                    <> GuiState.updateWidgetState (HoleWidgetIds.hidOpen widgetIds) newSearchTerm
                    -- When first letter is typed in search term, jump to the
                    -- results, which will go to first result:
                    & ( if Text.null searchTerm
                        then
                            GuiState.uCursor .~
                            Monoid.Last (Just (hidResultsPrefix widgetIds))
                        else id
                    )
        TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? hidOpenSearchTerm widgetIds
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ disallowChars holeKind fst
            <&> Align.tValue . Lens.mapped %~ pure . onEvents
            <&> Draw.backgroundColor bgAnimId (bgColor holeTheme)
    where
        bgAnimId = Widget.toAnimId (hidOpenSearchTerm widgetIds) <> ["hover background"]
