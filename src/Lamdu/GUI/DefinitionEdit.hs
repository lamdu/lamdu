module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui, assignCursor)
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

undeleteButton ::
    ( Monad i, Monad o
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    o Widget.Id -> ExprGuiM env i o (TextWidget o)
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
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Gui EventMap o ->
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.DefinitionExpression (Name o) i o
    (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM env i o (Gui Responsive o)
makeExprDefinition defEventMap def bodyExpr =
    AssignmentEdit.make (bodyExpr ^. Sugar.dePresentationMode) defEventMap
    (def ^. Sugar.drName) TextColors.definitionColor
    (bodyExpr ^. Sugar.deContent)
    & assignCursor myId (WidgetIds.fromEntityId (def ^. Sugar.drName . Sugar.tagRefTag . Sugar.tagInstance))
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

makeBuiltinDefinition ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.DefinitionBuiltin (Name g) o ->
    ExprGuiM env i o (TextWidget o)
makeBuiltinDefinition def builtin =
    TagEdit.makeBinderTagEdit TextColors.definitionColor name
    /|/ Label.make " = "
    /|/ BuiltinEdit.make builtin myId
    /-/ ( topLevelSchemeTypeView (builtin ^. Sugar.biType)
            & Reader.local (Element.animIdPrefix .~ animId ++ ["builtinType"])
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
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Gui EventMap o ->
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM env i o (Gui Responsive o)
make defEventMap def =
    do
        defGui <-
            case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition defEventMap def bodyExpr
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
                            & Responsive.alignedWidget . Align.tValue .> Widget.wFocused %@~ wholeFocused
                            & style
                    Responsive.vbox ?? [buttonGui, defGuiStyled]
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        defStateProp = def ^. Sugar.drDefinitionState
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView ::
    ( Monad i
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Glue.HasTexts env
    ) =>
    Sugar.Scheme (Name g) -> ExprGuiM env i o (WithTextPos View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
