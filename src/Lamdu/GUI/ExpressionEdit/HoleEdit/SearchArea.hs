{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( makeStdWrapped
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Monoid as Monoid
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as HoleEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
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

-- Has an ExpressionGui.stdWrap/typeView under the search term
makeStdWrapped ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload (T m) ExprGuiT.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> ExpressionGui m)
makeStdWrapped hole pl widgetIds =
    do
        config <- Lens.view Config.config
        let fdWrap
                | isAHoleInHole = return id
                | otherwise =
                    FocusDelegator.make ?? fdConfig (Config.hole config)
                    ?? FocusDelegator.FocusEntryParent ?? hidClosedSearchArea widgetIds
                    <&> (Align.tValue %~)
        isActive <- WidgetIds.isActive widgetIds
        let fixEventMapCursor
                | isActive = id
                | otherwise =
                    Lens.mapped . Lens.mapped . Widget.eCursor %~
                    mappend (Monoid.Last (Just (hidOpen widgetIds)))
        stateProp <-
            HoleState.assocStateRef (hole ^. Sugar.holeActions . Sugar.holeUUID)
            ^. Transaction.mkProperty & transaction
        let holeInfo =
                HoleInfo
                { hiEntityId = pl ^. Sugar.plEntityId
                , hiState = stateProp
                , hiInferredType = pl ^. Sugar.plAnnotation . Sugar.aInferredType
                , hiHole = hole
                , hiIds = widgetIds
                , hiNearestHoles = pl ^. Sugar.plData . ExprGuiT.plNearestHoles
                , hiMinOpPrec = pl ^. Sugar.plData . ExprGuiT.plMinOpPrec
                }
        closedSearchTermGui <-
            fdWrap <*> SearchTerm.make holeInfo <&> Responsive.fromWithTextPos
            & ExpressionGui.stdWrap pl
        eventMap <-
            sequence
            [ HoleEventMap.makeOpenEventMap holeInfo <&> fixEventMapCursor
            , ExprEventMap.make pl ExprGuiM.NoHolePick
            ] <&> mconcat
        case (isActive, isAHoleInHole) of
            (True, False) ->
                -- ideally the fdWrap would be "inside" the
                -- type-view addition and stdWrap, but it's not
                -- important in the case the FD is selected, and
                -- it is harder to implement, so just wrap it
                -- here
                (fdWrap <&> (Lens.mapped %~))
                <*> makeOpenSearchAreaGui holeInfo
                <&> Lens.mapped %~
                \open ->
                closedSearchTermGui & Responsive.alignedWidget . Align.tValue %~
                Hover.hoverInPlaceOf [Hover.anchor (open ^. Align.tValue)] . Hover.anchor
            (True, True) -> Widget.setFocused closedSearchTermGui & const & pure
            (False, _) -> const closedSearchTermGui & pure
            <&> Lens.mapped %~ E.weakerEvents eventMap
    where
        isAHoleInHole = ExprGuiT.isHoleResult pl
