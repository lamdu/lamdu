{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( makeStdWrapped
    ) where

import           Control.Lens.Operators
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout(..))
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

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
    Sugar.Payload m ExprGuiT.Payload -> HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeStdWrapped pl holeInfo =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
            WidgetIds{..} = hiIds holeInfo
            fdWrap =
                ExpressionGui.makeFocusDelegator (fdConfig (Config.hole config))
                FocusDelegator.FocusEntryChild hidClosedSearchArea
        closedSearchTermGui <-
            ExpressionGui.stdWrap pl <*> (fdWrap <*> SearchTerm.make holeInfo)
        isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
        if isSelected
            then -- ideally the fdWrap would be "inside" the
                 -- type-view addition and stdWrap, but it's not
                 -- important in the case the FD is selected, and
                 -- it is harder to implement, so just wrap it
                 -- here
                 fdWrap <*> makeOpenSearchAreaGui pl holeInfo
                 <&>
                 \gui ->
                 TreeLayout $
                 \layout ->
                 (gui ^. TreeLayout.render) layout
                 `AlignedWidget.hoverInPlaceOf`
                 (closedSearchTermGui ^. TreeLayout.render) layout
            else return closedSearchTermGui
