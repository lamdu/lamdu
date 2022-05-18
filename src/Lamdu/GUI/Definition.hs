{-# LANGUAGE RankNTypes #-}

module Lamdu.GUI.Definition
    ( make
    , module Lamdu.GUI.Definition.Result
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.CurAndPrev (CurPrevTag(..), fallbackToPrev, curPrevTag)
import           Hyper (annValue)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.Definition.Result
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import           Lamdu.GUI.Styled (label)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

resultWidget ::
    _ =>
    M.WidgetId -> Sugar.VarInfo -> CurPrevTag -> Sugar.EvalCompletionResult o ->
    m (M.TextWidget (DefRes o))
resultWidget myId varInfo tag res =
    case res of
    Sugar.EvalSuccess ->
        do
            view <- makeIndicator tag Theme.successColor "✔"
            toDoc <- Lens.view has <&> E.toDoc
            case varInfo of
                Sugar.VarNominal (Sugar.TId _ tid _) | tid == Builtins.mutTid ->
                    do
                        actionKeys <- Lens.view (has . Config.actionKeys)
                        let executeEventMap =
                                Lens._Wrapped # True & tell & DefRes
                                & E.keysEventMap actionKeys (toDoc [Texts.execRepl])
                        Widget.makeFocusableView ?? myId <&> (M.tValue %~) ?? view
                            <&> M.tValue %~ Widget.takesStroll myId
                            <&> M.tValue %~ Widget.weakerEvents executeEventMap
                _ -> view & M.tValue %~ Widget.fromView & pure
    Sugar.EvalError err ->
        errorIndicator myId tag err
        <&> M.tValue . Widget.updates %~ lift

indicatorColor :: _ => CurPrevTag -> Lens.ALens' Theme M.Color -> m M.Color
indicatorColor Current color = Lens.view (has . Lens.cloneLens color)
indicatorColor Prev _ = Lens.view (has . Theme.disabledColor)

makeIndicator :: _ => CurPrevTag -> Lens.ALens' Theme M.Color -> Text -> m (M.WithTextPos M.View)
makeIndicator tag enabledColor text =
    do
        color <- indicatorColor tag enabledColor
        Label.make text & local (TextView.color .~ color)

compiledErrorDesc :: Sugar.CompiledErrorType -> OneOf Texts.CodeUI
compiledErrorDesc Sugar.ReachedHole = Texts.jsReachedAHole
compiledErrorDesc Sugar.DependencyTypeOutOfDate = Texts.jsStaleDep
compiledErrorDesc Sugar.UnhandledCase = Texts.jsUnhandledCase

errorDesc :: _ => Sugar.Error -> m (M.WithTextPos M.View)
errorDesc err =
    do
        errorColor <- Lens.view (has . Theme.errorColor)
        case err of
            Sugar.CompiledError cErr ->
                label (compiledErrorDesc cErr)
            Sugar.RuntimeError exc ->
                label Texts.jsException
                M./|/ ((TextView.make ?? exc)
                        <*> (Element.subAnimId ?? ["exception text"]))
            & local (TextView.color .~ errorColor)

errorIndicator :: _ => Widget.Id -> CurPrevTag -> Sugar.EvalException o -> m (M.TextWidget o)
errorIndicator myId tag (Sugar.EvalException errorType jumpToErr) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        env <- Lens.view id
        let jumpDoc =
                E.toDoc env
                [has . MomentuTexts.navigation, has . Texts.jumpToError]
        let jumpEventMap j =
                j <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor actionKeys jumpDoc
        indicator <-
            (Widget.makeFocusableView ?? myId <&> (M.tValue %~))
            <*> makeIndicator tag Theme.errorColor "⚠"
            <&> Lens.mapped %~ Widget.takesStroll myId
            <&> Lens.mapped %~ Widget.weakerEvents (foldMap jumpEventMap jumpToErr)
        if Widget.isFocused (indicator ^. M.tValue)
            then
            do
                descLabel <- errorDesc errorType
                hspace <- Spacer.stdHSpace
                vspace <- Spacer.stdVSpace
                hover <- Hover.hover
                Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
                Glue.Poly (|---|) <- Glue.mkPoly ?? Glue.Vertical
                anchor <- Hover.anchor <&> fmap
                let hDescLabel f = hover (f descLabel) & Hover.sequenceHover
                let hoverOptions =
                        [ anchor indicator ||| hDescLabel (hspace |||)
                        , anchor indicator |---| hDescLabel (vspace |---|)
                        ] <&> (^. M.tValue)
                anchor indicator
                    <&> Hover.hoverInPlaceOf hoverOptions
                    & pure
            else
                pure indicator

makeExprDefinition ::
    _ =>
    Sugar.OptionalTag Name i o ->
    ExprGui.Top Sugar.DefinitionExpression i o ->
    M.WidgetId ->
    GuiM env i o (Responsive (DefRes o))
makeExprDefinition defName bodyExpr myId =
    case bodyExpr ^. Sugar.deContent . hVal of
    Sugar.BodyPlain x | Lens.has (Sugar.oTag . Sugar.tagRefJumpTo . Lens._Nothing) defName ->
        do
            isPickingName <- GuiState.isSubCursor ?? pickNameId
            lhs <-
                if isPickingName
                then
                    do
                        nameEdit <- makeNameEdit <&> Responsive.fromWithTextPos
                        AssignmentEdit.makePlainLhs nameEdit (x ^. Sugar.apAddFirstParam)
                            (WidgetIds.fromExprPayload (bodyExpr ^. Sugar.deContent . annotation))
                        <&> Lens.mapped . Widget.updates %~ lift
                else
                    do
                        nameEventMap <- TagEdit.makeChooseEventMap pickNameId
                        ((Widget.makeFocusableView ?? pickNameId <&> (M.tValue %~)) <*> label Texts.repl)
                            Glue./-/
                            ( (resultWidget indicatorId (bodyExpr ^. Sugar.deVarInfo) <$> curPrevTag <&> fmap) <*> bodyExpr ^. Sugar.deResult
                                & fallbackToPrev
                                & fromMaybe (Widget.respondToCursorPrefix ?? indicatorId ?? M.empty <&> M.WithTextPos 0)
                                & local (M.animIdPrefix <>~ ["result widget"])
                            )
                            <&> Responsive.fromWithTextPos
                            <&> M.weakerEvents nameEventMap
                            <&> (:[])
            bodyExpr ^. Sugar.deContent & annValue .~ x ^. Sugar.apBody & GuiM.makeBinder
                <&> Widget.updates %~ lift
                >>= AssignmentEdit.layout lhs
            & GuiState.assignCursor myId (WidgetIds.tagHoleId pickNameId)
        where
            indicatorId = Widget.joinId myId ["result indicator"]
            pickNameId =
                defName ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance
                & WidgetIds.fromEntityId
    _ ->
        do
            mPresentationEdit <-
                do
                    presModeProp <- bodyExpr ^. Sugar.dePresentationMode
                    params <- bodyExpr ^? Sugar.deContent . hVal . Sugar._BodyFunction . Sugar.fParams
                    GuiM.im presModeProp >>= PresentationModeEdit.make presentationChoiceId params & Just
                & sequenceA
            (|---|) <- Glue.mkGlue ?? Glue.Vertical
            makeNameEdit <&> (|---| fromMaybe M.empty mPresentationEdit) <&> Responsive.fromWithTextPos
                >>= AssignmentEdit.make nameEditId (bodyExpr ^. Sugar.deContent)
        & GuiState.assignCursor myId nameEditId
        <&> Widget.updates %~ lift
    where
        makeNameEdit = TagEdit.makeBinderTagEdit TextColors.definitionColor defName
        nameEditId = defName ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
        presentationChoiceId = Widget.joinId myId ["presentation"]

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
    GuiM env i o (Responsive (DefRes o))
make def myId =
    do
        env <- Lens.view id
        let nextOutdated =
                E.keyPresses
                (env ^. has . Config.pane . Config.nextOutdatedKeys)
                (E.Doc [env ^. has . Texts.gotoNextOutdated])
                (def ^. Sugar.drGotoNextOutdated
                    <&> foldMap (GuiState.updateCursor . WidgetIds.fromEntityId))
                <&> lift
        case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition (def ^. Sugar.drName) bodyExpr myId
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin myId <&> Responsive.fromWithTextPos
                <&> Widget.updates %~ lift
            <&> M.weakerEvents nextOutdated
    & local (M.animIdPrefix .~ Widget.toAnimId myId)

topLevelSchemeTypeView :: _ => Sugar.Scheme Name m -> GuiM env i o (M.WithTextPos M.View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
