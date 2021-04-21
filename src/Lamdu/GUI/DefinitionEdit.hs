module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Definitions as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

undeleteButton :: _ => o Widget.Id -> GuiM env i o (M.TextWidget o)
undeleteButton undelete =
    do
        actionId <- Element.subAnimId ?? ["Undelete"] <&> Widget.Id
        toDoc <- Lens.view id <&> E.toDoc
        let doc =
                toDoc
                [ has . MomentuTexts.edit
                , has . Texts.def
                , has . Texts.undelete
                ]
        Styled.actionable actionId Texts.undeleteButton
            doc undelete

makeExprDefinition ::
    _ =>
    ExprGui.Top Sugar.Definition i o ->
    ExprGui.Top Sugar.DefinitionExpression i o ->
    GuiM env i o (Responsive o)
makeExprDefinition def bodyExpr =
    AssignmentEdit.make (bodyExpr ^. Sugar.dePresentationMode)
    (def ^. Sugar.drName) TextColors.definitionColor
    (bodyExpr ^. Sugar.deContent)
    & GuiState.assignCursor myId
        (WidgetIds.fromEntityId (def ^. Sugar.drName . Sugar.tagRefTag . Sugar.tagInstance))
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

makeBuiltinDefinition ::
    _ =>
    Sugar.Definition v Name i o (Sugar.Payload v Name i o, Sugar.GuiPayload) ->
    Sugar.DefinitionBuiltin Name o ->
    GuiM env i o (M.TextWidget o)
makeBuiltinDefinition def builtin =
    TagEdit.makeBinderTagEdit TextColors.definitionColor name
    M./|/ Label.make " = "
    M./|/ BuiltinEdit.make builtin myId
    M./-/ ( topLevelSchemeTypeView (builtin ^. Sugar.biType)
            & Reader.local (M.animIdPrefix .~ animId ++ ["builtinType"])
        )
    where
        name = def ^. Sugar.drName
        animId = myId & Widget.toAnimId
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

wholeFocused :: Widget.Size -> Widget.Focused a -> Widget.Focused a
wholeFocused size f =
    Widget.Focused
    { Widget._fFocalAreas = [Rect 0 size]
    , Widget._fEventMap = mempty
    , Widget._fPreEvents = mempty
    , Widget._fMEnterPoint = Nothing
    , Widget._fLayers = f ^. Widget.fLayers
    }

make ::
    _ => EventMap (o GuiState.Update) -> ExprGui.Top Sugar.Definition i o -> GuiM env i o (Responsive o)
make defEventMap def =
    do
        defGui <-
            case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition def bodyExpr
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin <&> Responsive.fromWithTextPos
            <&> Widget.weakerEvents defEventMap
        case defStateProp ^. Property.pVal of
            Sugar.LiveDefinition -> pure defGui
            Sugar.DeletedDefinition ->
                do
                    buttonGui <-
                        myId <$ Property.set defStateProp Sugar.LiveDefinition
                        & undeleteButton <&> Responsive.fromWithTextPos
                    style <- Styled.deletedDef
                    let defGuiStyled =
                            defGui
                            & Responsive.alignedWidget . M.tValue .> Widget.wFocused %@~ wholeFocused
                            & style
                    Responsive.vbox ?? [buttonGui, defGuiStyled]
    & Reader.local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        defStateProp = def ^. Sugar.drDefinitionState
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView :: _ => Sugar.Scheme Name -> GuiM env i o (M.WithTextPos M.View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
