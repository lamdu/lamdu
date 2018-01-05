{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Widget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import           Lamdu.Config (HasConfig)
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme

import           Lamdu.Prelude

make ::
    ( MonadReader env m, HasTheme env , HasConfig env, TextEdit.HasStyle env, GuiState.HasState env
    , Applicative f
    ) =>
    Widget.Id -> (Text -> Bool) -> m (WithTextPos (Widget (f GuiState.Update)))
make searchMenuId allowedSearchTerm =
    do
        isActive <- GuiState.isSubCursor ?? searchMenuId
        let bgColor
                | isActive = Theme.holeActiveSearchTermBGColor
                | otherwise = Theme.holeSearchTermBGColor
        theme <- Lens.view Theme.theme <&> Theme.hole
        SearchMenu.basicSearchTermEdit searchMenuId allowedSearchTerm
            <&> Align.tValue . Lens.mapped %~ pure
            <&> Draw.backgroundColor
                (Widget.toAnimId searchMenuId <> ["hover background"])
                (bgColor theme)
