{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( makeStdWrapped
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as HoleEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
import           Lamdu.GUI.ExpressionGui.HolePicker (HolePicker(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

fdConfig :: Config.Hole -> FocusDelegator.Config
fdConfig Config.Hole{holeOpenKeys, holeCloseKeys} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = holeOpenKeys
    , FocusDelegator.focusChildDoc = M.Doc ["Navigation", "Hole", "Open"]
    , FocusDelegator.focusParentKeys = holeCloseKeys
    , FocusDelegator.focusParentDoc = M.Doc ["Navigation", "Hole", "Close"]
    }

-- Has an stdWrap/typeView under the search term
makeStdWrapped ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> ExpressionGui m)
makeStdWrapped hole pl widgetIds =
    do
        config <- Lens.view Config.config
        let unwrapAsEventMap =
                hole ^? Sugar.holeKind . Sugar._WrapperHole . Sugar.haUnwrap . Sugar._UnwrapAction
                & maybe mempty
                    ( Widget.keysEventMapMovesCursor (Config.delKeys config <> Config.holeUnwrapKeys (Config.hole config))
                        (M.Doc ["Edit", "Unwrap"])
                        . fmap WidgetIds.fromEntityId
                    )
        let fdWrap =
                FocusDelegator.make ?? fdConfig (Config.hole config)
                ?? FocusDelegator.FocusEntryParent ?? hidClosedSearchArea widgetIds
                <&> (M.tValue %~)
        closedSearchTermGui <-
            (if isAHoleInHole then return id else fdWrap)
            <*> SearchTerm.make widgetIds holeKind <&> Responsive.fromWithTextPos
            <&> M.weakerEvents unwrapAsEventMap
            & stdWrap pl
        isActive <- WidgetIds.isActive widgetIds
        searchTermEventMap <-
            HoleEventMap.makeSearchTermEditEventMap holeKind widgetIds <&>
            if isActive
            then id
            else
                Lens.mapped . Lens.mapped . GuiState.uCursor %~
                mappend (Monoid.Last (Just (hidOpen widgetIds)))
        exprEventMap <- ExprEventMap.make (pl & Sugar.plData . ExprGui.plMinOpPrec .~ 100) NoHolePick
        case (isActive, isAHoleInHole) of
            (True, False) ->
                -- ideally the fdWrap would be "inside" the
                -- type-view addition and stdWrap, but it's not
                -- important in the case the FD is selected, and
                -- it is harder to implement, so just wrap it
                -- here
                (fdWrap <&> (Lens.mapped %~))
                <*> makeOpenSearchAreaGui hole pl widgetIds
                <&> Lens.mapped %~ inPlaceOfClosed . M.weakerEvents unwrapAsEventMap . (^. M.tValue)
                where
                    inPlaceOfClosed open =
                        closedSearchTermGui & M.widget %~
                        Hover.hoverInPlaceOf [Hover.anchor open] . Hover.anchor
            (True, True) ->
                Widget.setFocused closedSearchTermGui
                & M.weakerEvents searchTermEventMap
                & const & pure
            (False, _) ->
                closedSearchTermGui
                & M.weakerEvents searchTermEventMap
                & const & pure
            <&> Lens.mapped %~ M.weakerEvents exprEventMap
    where
        isAHoleInHole = ExprGui.isHoleResult pl
        holeKind = hole ^. Sugar.holeKind
