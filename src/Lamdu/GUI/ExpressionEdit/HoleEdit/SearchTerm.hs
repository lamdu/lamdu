{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
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
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
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
makeSearchTermPropEdit WidgetIds{..} searchTermProp =
    TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? hidOpenSearchTerm
    <&> Align.tValue . Widget.events %~ \(newSearchTerm, eventRes) ->
        do
            when (newSearchTerm /= searchTerm) $
                Property.set searchTermProp newSearchTerm
            eventRes
                -- When first letter is typed in search term, jump to the
                -- results, which will go to first result:
                & ( if Text.null searchTerm && (not . Text.null) newSearchTerm
                    then Widget.eCursor .~ Monoid.Last (Just hidResultsPrefix)
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
        let holeConfig@Config.Hole{..} = Config.hole config
        let Theme.Hole{..} = Theme.hole theme
        textCursor <- TextEdit.getCursor ?? searchTerm ?? hidOpenSearchTerm
        makeSearchTermPropEdit WidgetIds{..} (HoleInfo.hiSearchTermProperty holeInfo)
            <&> Align.tValue . E.eventMap
                %~ EventMap.disallowCharsFromSearchTerm holeConfig holeInfo textCursor
            <&> addBackground (Widget.toAnimId hidOpenSearchTerm) holeSearchTermBGColor
    where
        WidgetIds{..} = hiIds holeInfo
        searchTerm = HoleInfo.hiSearchTerm holeInfo
