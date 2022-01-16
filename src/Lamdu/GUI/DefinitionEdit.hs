module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeExprDefinition ::
    _ =>
    ExprGui.Top Sugar.Definition i o ->
    ExprGui.Top Sugar.DefinitionExpression i o ->
    M.WidgetId ->
    GuiM env i o (Responsive o)
makeExprDefinition def bodyExpr myId =
    TagEdit.makeBinderTagEdit TextColors.definitionColor (def ^. Sugar.drName)
    <&> Responsive.fromWithTextPos
    >>= AssignmentEdit.make (bodyExpr ^. Sugar.dePresentationMode) nameEditId (bodyExpr ^. Sugar.deContent)
    & GuiState.assignCursor myId nameEditId
    where
        nameEditId =
            def ^. Sugar.drName . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeBuiltinDefinition ::
    _ =>
    Sugar.Definition v Name i o (Sugar.Payload v o) ->
    Sugar.DefinitionBuiltin Name o ->
    M.WidgetId ->
    GuiM env i o (M.TextWidget o)
makeBuiltinDefinition def builtin myId =
    TagEdit.makeBinderTagEdit TextColors.definitionColor name
    M./|/ Label.make " = "
    M./|/ BuiltinEdit.make builtin myId
    M./-/ ( topLevelSchemeTypeView (builtin ^. Sugar.biType)
            & local (M.animIdPrefix .~ animId ++ ["builtinType"])
        )
    where
        name = def ^. Sugar.drName
        animId = myId & Widget.toAnimId

make ::
    _ =>
    ExprGui.Top Sugar.Definition i o ->
    M.WidgetId ->
    GuiM env i o (Responsive o)
make def myId =
    do
        env <- Lens.view id
        let nextOutdated =
                E.keyPresses
                (env ^. has . Config.pane . Config.nextOutdatedKeys)
                (E.Doc [env ^. has . Texts.gotoNextOutdated])
                (def ^. Sugar.drGotoNextOutdated
                    <&> foldMap (GuiState.updateCursor . WidgetIds.fromEntityId))
        case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition def bodyExpr myId
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin myId <&> Responsive.fromWithTextPos
            <&> M.weakerEvents nextOutdated
    & local (M.animIdPrefix .~ Widget.toAnimId myId)

topLevelSchemeTypeView :: _ => Sugar.Scheme Name Unit -> GuiM env i o (M.WithTextPos M.View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
