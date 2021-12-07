module Lamdu.GUI.NominalPane
    ( make
    ) where

import qualified GUI.Momentu as M
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as ResponsiveOptions
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => Sugar.NominalPane Name i o -> GuiM env i o (Responsive o)
make nom =
    do
        nameEdit <-
            TagEdit.makeBinderTagEdit TextColors.nomColor (nom ^. Sugar.npName)
            <&> Responsive.fromWithTextPos
        hbox <- ResponsiveOptions.boxSpaced ?? ResponsiveOptions.disambiguationNone
        paramEdits <-
            nom ^. Sugar.npParams & traverse (TagEdit.makeRecordTag . (^. Sugar.pName))
            <&> map Responsive.fromWithTextPos
        sep <- Styled.grammar (Label.make ":") <&> Responsive.fromTextView
        bodyEdit <- makeNominalPaneBody (nom ^. Sugar.npBody)
        hbox [hbox ((nameEdit : paramEdits) <> [sep]), bodyEdit]
            & pure
        & local (M.animIdPrefix .~ Widget.toAnimId myId)
        & GuiState.assignCursor myId nameEditId
    where
        myId = nom ^. Sugar.npEntityId & WidgetIds.fromEntityId
        nameEditId =
            nom ^. Sugar.npName . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance
            & WidgetIds.fromEntityId

makeNominalPaneBody :: _ => Maybe (Sugar.Scheme Name o) -> f (Responsive a)
makeNominalPaneBody Nothing =
    Styled.grammar (Styled.focusableLabel Texts.opaque)
    <&> Responsive.fromWithTextPos
makeNominalPaneBody (Just scheme) = TypeView.makeScheme scheme <&> Responsive.fromTextView
