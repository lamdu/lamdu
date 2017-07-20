{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( makeStdWrapped
    ) where

import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui, ResultsPlacement)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

fdConfig :: Config.Hole -> FocusDelegator.Config
fdConfig Config.Hole{..} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = holeOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Hole", "Open"]
    , FocusDelegator.focusParentKeys = holeCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Hole", "Close"]
    }

-- Has an ExpressionGui.stdWrap/typeView under the search term
makeStdWrapped ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> HoleInfo m ->
    ExprGuiM m (ResultsPlacement -> ExpressionGui m)
makeStdWrapped pl holeInfo =
    do
        config <- Lens.view Config.config
        let Config.Hole{..} = Config.hole config
            WidgetIds{..} = hiIds holeInfo
            fdWrap
                | ExprGuiT.plOfHoleResult pl = return id
                | otherwise =
                    FocusDelegator.make ?? fdConfig (Config.hole config)
                    ?? FocusDelegator.FocusEntryChild ?? hidClosedSearchArea
                    <&> (Align.tValue %~)
        closedSearchTermGui <-
            fdWrap <*> SearchTerm.make holeInfo <&> TreeLayout.fromWithTextPos
            & ExpressionGui.stdWrap pl
        isSelected <- Widget.isSubCursor ?? hidOpen
        if isSelected
            then -- ideally the fdWrap would be "inside" the
                 -- type-view addition and stdWrap, but it's not
                 -- important in the case the FD is selected, and
                 -- it is harder to implement, so just wrap it
                 -- here
                 (fdWrap <&> (Lens.mapped %~))
                 <*> makeOpenSearchAreaGui pl holeInfo
                 <&> Lens.mapped %~
                 \open ->
                 closedSearchTermGui & TreeLayout.alignedWidget . Align.tValue %~
                 Align.hoverInPlaceOf [Align.anchor (open ^. Align.tValue)] . Align.anchor
            else return (const closedSearchTermGui)
