module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

undeleteButton ::
    (Monad i, Monad o) =>
    o Widget.Id -> ExprGuiM i o (WithTextPos (Widget (o GuiState.Update)))
undeleteButton undelete =
    do
        actionId <- Element.subAnimId ["Undelete"] <&> Widget.Id
        Styled.actionable actionId "Undelete..." doc undelete
    where
        doc = E.Doc ["Edit", "Undelete definition"]

makeExprDefinition ::
    (Monad i, Monad o) =>
    EventMap (o GuiState.Update) ->
    Sugar.Definition (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.DefinitionExpression (Name o) i o (ExprGui.SugarExpr i o) ->
    ExprGuiM i o (ExpressionGui o)
makeExprDefinition lhsEventMap def bodyExpr =
    BinderEdit.make (bodyExpr ^. Sugar.dePresentationMode) lhsEventMap
    (def ^. Sugar.drName) TextColors.definitionColor
    (bodyExpr ^. Sugar.deContent) myId
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

makeBuiltinDefinition ::
    (Monad i, Monad o) =>
    Sugar.Definition (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.DefinitionBuiltin (Name g) o ->
    ExprGuiM i o (WithTextPos (Widget (o GuiState.Update)))
makeBuiltinDefinition def builtin =
    do
        nameEdit <- TagEdit.makeBinderTagEdit TextColors.definitionColor name
        equals <- TextView.makeLabel " = "
        builtinEdit <- BuiltinEdit.make builtin myId
        typeView <-
            topLevelSchemeTypeView (builtin ^. Sugar.biType)
            & Reader.local (Element.animIdPrefix .~ animId ++ ["builtinType"])
        (nameEdit /|/ equals /|/ builtinEdit)
            /-/
            typeView
            & pure
    where
        name = def ^. Sugar.drName
        animId = myId & Widget.toAnimId
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

make ::
    (Monad i, Monad o) =>
    EventMap (o GuiState.Update) ->
    Sugar.Definition (Name o) i o (ExprGui.SugarExpr i o) ->
    ExprGuiM i o (ExpressionGui o)
make lhsEventMap def =
    do
        defStateProp <- def ^. Sugar.drDefinitionState & ExprGuiM.im
        let defState = Property.value defStateProp
        addDeletionDiagonal <-
            case defState of
            Sugar.DeletedDefinition -> Styled.addDeletionDiagonal ?? 0.02
            Sugar.LiveDefinition -> pure id
        defGui <-
            case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition lhsEventMap def bodyExpr
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents lhsEventMap
            <&> addDeletionDiagonal
        case defState of
            Sugar.LiveDefinition -> pure defGui
            Sugar.DeletedDefinition ->
                do
                    buttonGui <-
                        myId <$ Property.set defStateProp Sugar.LiveDefinition
                        & undeleteButton <&> Responsive.fromWithTextPos
                    Responsive.vbox [defGui, buttonGui] & pure
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView ::
    Monad i => Sugar.Scheme (Name g) -> ExprGuiM i o (WithTextPos View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
