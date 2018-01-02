{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as HoleEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

fdConfig :: Config.Hole -> FocusDelegator.Config
fdConfig Config.Hole{holeOpenKeys, holeCloseKeys} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = holeOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Hole", "Open"]
    , FocusDelegator.focusParentKeys = holeCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Hole", "Close"]
    }

-- Has a typeView under the search term
make ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (Menu.Placement -> ExpressionGui m)
make hole pl =
    do
        config <- Lens.view Config.config
        let fdWrap =
                FocusDelegator.make ?? fdConfig (Config.hole config)
                ?? FocusDelegator.FocusEntryParent ?? hidClosedSearchArea widgetIds
                <&> (Align.tValue %~)
        closedSearchTermGui <-
            maybeAddAnnotationPl pl
            <*>
            ( fdWrap
                <*> SearchTerm.make widgetIds holeKind <&> Responsive.fromWithTextPos
            )
        isActive <- HoleWidgetIds.isActive widgetIds
        searchTermEventMap <- HoleEventMap.searchTermEditEventMap widgetIds ?? holeKind <&> fmap pure
        let inPlaceOfClosed open =
                closedSearchTermGui & Widget.widget %~
                Hover.hoverInPlaceOf [Hover.anchor open] . Hover.anchor
        if isActive && not isAHoleInHole
            then
                -- ideally the fdWrap would be "inside" the
                -- type-view addition and stdWrap, but it's not
                -- important in the case the FD is selected, and
                -- it is harder to implement, so just wrap it
                -- here
                (fdWrap <&> (Lens.mapped %~))
                <*> makeOpenSearchAreaGui searchTermEventMap hole pl
                <&> Lens.mapped %~ inPlaceOfClosed . (^. Align.tValue)
            else
                (if isActive then Widget.setFocused else id)
                closedSearchTermGui
                & Widget.weakerEvents
                  (-- Editing search term of a closed hole opens it:
                      searchTermEventMap <&> Lens.mapped . GuiState.uCursor %~
                      mappend (Monoid.Last (Just (hidOpen widgetIds)))
                  )
                & const & pure
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        isAHoleInHole = ExprGui.isHoleResult pl
        holeKind = hole ^. Sugar.holeKind
