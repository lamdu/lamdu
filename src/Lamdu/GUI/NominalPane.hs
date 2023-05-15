module Lamdu.GUI.NominalPane
    ( make
    ) where

import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as ResponsiveOptions
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeParams as TypeParams
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => Sugar.NominalPane Name i o -> GuiM env i o (Responsive o)
make nom =
    do
        (addFirstEventMap, paramEdits) <-
            TypeParams.make (nom ^. Sugar.npParams) (pure nameEditId) myId
        nameEdit <-
            TagEdit.makeBinderTagEdit TextColors.nomColor (nom ^. Sugar.npName)
            <&> Responsive.fromWithTextPos
            <&> M.weakerEvents addFirstEventMap
        sep <- Styled.grammar (Label.make ":") <&> Responsive.fromTextView
        sequenceA
            [ hbox ((nameEdit : paramEdits) <> [sep])
            , case nom ^. Sugar.npBody of
                Nothing -> Styled.grammar (Styled.focusableLabel Texts.opaque) <&> Responsive.fromWithTextPos
                Just scheme -> TypeView.makeScheme scheme <&> Responsive.fromTextView
            ] >>= hbox
        & local (M.elemIdPrefix .~ M.asElemId myId)
        & GuiState.assignCursor myId nameEditId
    where
        hbox = ResponsiveOptions.boxSpaced ResponsiveOptions.disambiguationNone
        myId = nom ^. Sugar.npEntityId & WidgetIds.fromEntityId
        nameEditId =
            nom ^. Sugar.npName . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance
            & WidgetIds.fromEntityId
