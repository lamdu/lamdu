module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui, assignCursor)
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

undeleteButton ::
    (Monad i, Monad o) =>
    o Widget.Id -> ExprGuiM i o (TextWidget o)
undeleteButton undelete =
    do
        actionId <- Element.subAnimId ?? ["Undelete"] <&> Widget.Id
        Styled.actionable actionId (Texts.codeUI . Texts.undeleteButton) doc undelete
    where
        doc = E.Doc ["Edit", "Definition", "Undelete"]

makeExprDefinition ::
    (Monad i, Monad o) =>
    Gui EventMap o ->
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.DefinitionExpression (Name o) i o
    (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM i o (Gui Responsive o)
makeExprDefinition defEventMap def bodyExpr =
    AssignmentEdit.make (bodyExpr ^. Sugar.dePresentationMode) defEventMap
    (def ^. Sugar.drName) TextColors.definitionColor
    (bodyExpr ^. Sugar.deContent)
    & assignCursor myId (WidgetIds.fromEntityId (def ^. Sugar.drName . Sugar.tagInfo . Sugar.tagInstance))
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

makeBuiltinDefinition ::
    (Monad i, Monad o) =>
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.DefinitionBuiltin (Name g) o ->
    ExprGuiM i o (TextWidget o)
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
    (Monad i, Monad o) =>
    Gui EventMap o ->
    Sugar.Definition (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM i o (Gui Responsive o)
make defEventMap def =
    do
        defStateProp <- def ^. Sugar.drDefinitionState & ExprGuiM.im
        let defState = Property.value defStateProp
        defGui <-
            case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition defEventMap def bodyExpr
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents defEventMap
        case defState of
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
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView ::
    Monad i => Sugar.Scheme (Name g) -> ExprGuiM i o (WithTextPos View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
