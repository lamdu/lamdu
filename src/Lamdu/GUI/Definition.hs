module Lamdu.GUI.Definition
    ( make
    , module Lamdu.GUI.Definition.Result
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.CurAndPrev (CurPrevTag(..), fallbackToPrev, curPrevTag)
import           Data.Property (Property, pVal, pSet)
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Hyper (annValue)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.Classes (InfoMonad(..))
import           Lamdu.GUI.Definition.Result
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

resultWidget ::
    _ =>
    ElemId -> Sugar.VarInfo -> CurPrevTag -> Sugar.EvalCompletionResult o ->
    m (M.TextWidget (DefRes o))
resultWidget myId varInfo tag res =
    case res of
    Sugar.EvalSuccess ->
        do
            view <- makeIndicator tag Theme.successColor "✔"
            toDoc <- Lens.view has <&> E.toDoc
            case varInfo of
                Sugar.VarNominal (Sugar.TId _ tid) | tid == Builtins.mutTid ->
                    do
                        actionKeys <- Lens.view (has . Config.actionKeys)
                        let executeEventMap =
                                Lens._Wrapped # True & tell & DefRes
                                & E.keysEventMap actionKeys (toDoc [Texts.execRepl])
                        view & M.tValue (Widget.makeFocusableView myId)
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

compiledErrorDesc :: Sugar.CompiledErrorType -> Lens.ALens' (Texts.CodeUI a) a
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
                M./|/ (Element.subElemId "exception text" >>= TextView.make exc)
            & local (TextView.color .~ errorColor)

errorIndicator :: _ => ElemId -> CurPrevTag -> Sugar.EvalException o -> m (M.TextWidget o)
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
            makeIndicator tag Theme.errorColor "⚠"
            >>= M.tValue (Widget.makeFocusableView myId)
            <&> Lens.mapped %~ Widget.takesStroll myId
            <&> Lens.mapped %~ Widget.weakerEvents (foldMap jumpEventMap jumpToErr)
        if Widget.isFocused (indicator ^. M.tValue)
            then
            do
                descLabel <- errorDesc errorType
                hspace <- Spacer.stdHSpace
                vspace <- Spacer.stdVSpace
                hover <- pushToReader Hover.hover
                Glue.Poly (|||) <- Glue.mkPoly Glue.Horizontal
                Glue.Poly (|---|) <- Glue.mkPoly Glue.Vertical
                anchor <- pushToReader Hover.anchor <&> fmap
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

makeAddResultWidget ::
    _ => ElemId -> Sugar.DefinitionExpression v name i o a -> Responsive (DefRes o) -> m (Responsive (DefRes o))
makeAddResultWidget myId bodyExpr w =
    (resultWidget indicatorId (bodyExpr ^. Sugar.deVarInfo) <$> curPrevTag <&> fmap) <*> bodyExpr ^. Sugar.deResult
    & fallbackToPrev
    & fromMaybe (Widget.respondToCursorPrefix indicatorId M.empty <&> M.WithTextPos 0)
    >>= (Glue.mkGlue Glue.Horizontal ?? w)
    & local (M.elemIdPrefix <>~ "result widget")
    where
        indicatorId = myId <> "result indicator"

makeOperatorIndicator ::
    _ => ElemId -> Maybe (Property f (Sugar.SpecialArgs a)) -> m (M.TextWidget f)
makeOperatorIndicator nameId mPresMode =
    case mPresMode of
    Just presMode | Lens.has (pVal . Sugar._Operator) presMode ->
        do
            delKeys <- Config.delKeys
            toDoc <- Lens.view has <&> E.toDoc
            let executeEventMap =
                    nameId <$ (presMode ^. pSet) Sugar.Verbose
                    & E.keysEventMapMovesCursor delKeys (toDoc [Texts.presentationMode, Texts.pModeVerbose])
            TextView.make "." opIndicatorId
                >>= M.tValue (Widget.makeFocusableView opIndicatorId)
                <&> M.tValue %~ M.weakerEvents executeEventMap
    _ -> pure M.empty
    where
        opIndicatorId = nameId <> M.ElemId ["."]

makeToOperatorEventMap ::
    _ =>
    Sugar.LhsNames name i o v ->
    Maybe (Property m (Sugar.SpecialArgs T.Tag)) ->
    f (E.EventMap (m a))
makeToOperatorEventMap params mPresMode =
    case (mPresMode, params ^.. Sugar._LhsRecord . SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagVal) of
    (Just presMode, l : r : _) | Lens.has (pVal . Sugar._Verbose) presMode ->
        Lens.view has <&>
        \env ->
        mempty <$ (presMode ^. pSet) (Sugar.Operator l r) & const
        & E.charGroup Nothing (E.toDoc env [Texts.presentationMode, Texts.pModeOperator]) "."
    _ -> pure mempty

makeExprDefinition ::
    _ =>
    Sugar.OptionalTag Name i o ->
    ExprGui.Top Sugar.DefinitionExpression i o ->
    ElemId ->
    GuiM env i o (Responsive (DefRes o))
makeExprDefinition defName bodyExpr myId =
    case bodyExpr ^. Sugar.deContent . hVal of
    Sugar.BodyPlain x ->
        do
            isPickingName <- GuiState.isSubCursor nameEditId
            let isPlainLhs = isPickingName || Lens.has (Sugar.oTag . Sugar.tagRefJumpTo . Lens._Just) defName

            let rhsId = bodyExpr ^. Sugar.deContent . annotation & WidgetIds.fromExprPayload
            lhsEventMap <- AssignmentEdit.makePlainLhsEventMap (x ^. Sugar.apAddFirstParam) rhsId
            nameEdit <-
                makeNameEdit <&> Responsive.fromWithTextPos
                & (if isPlainLhs then id else local (GuiState.cursor .~ nameTagHoleId))
                <&> Widget.weakerEvents lhsEventMap
                <&> Widget.updates %~ lift

            equals <- grammar (label Texts.assign) <&> Responsive.fromTextView
            plainLhs <- Options.boxSpaced Options.disambiguationNone [nameEdit, equals]
            let width =
                    plainLhs
                    ^. Responsive.rWide . Responsive.lWide . M.tValue . Widget.wSize . Lens._1
            lhs <-
                ( if isPlainLhs
                    then pure plainLhs
                    else
                        label Texts.repl >>= M.tValue (Widget.makeFocusableView nameTagHoleId)
                        >>= Element.padToSize (M.Vector2 width 0) 0
                        <&> Responsive.fromWithTextPos
                ) >>= makeAddResultWidget myId bodyExpr
            bodyExpr ^. Sugar.deContent & annValue .~ x ^. Sugar.apBody & GuiM.makeBinder
                <&> Widget.updates %~ lift
                >>= AssignmentEdit.layout lhs
    Sugar.BodyFunction f ->
        do
            mPresMode <- bodyExpr ^. Sugar.dePresentationMode & Lens._Just liftInfo
            toOp <- makeToOperatorEventMap (f ^. Sugar.fParams) mPresMode
            makeOperatorIndicator nameEditId mPresMode
                M./|/ (makeNameEdit <&> M.tValue %~ M.weakerEvents toOp) <&> Responsive.fromWithTextPos
                >>= AssignmentEdit.make nameEditId (bodyExpr ^. Sugar.deContent)
        <&> Widget.updates %~ lift
    & GuiState.assignCursor myId nameEditId
    where
        nameTagHoleId = WidgetIds.tagHoleId nameEditId
        makeNameEdit = TagEdit.makeBinderTagEdit TextColors.definitionColor defName
        nameEditId = defName ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeBuiltinDefinition ::
    _ =>
    Sugar.Definition v Name i o (Sugar.Payload v o) ->
    Sugar.DefinitionBuiltin Name o ->
    ElemId ->
    GuiM env i o (M.TextWidget o)
makeBuiltinDefinition def builtin myId =
    TagEdit.makeBinderTagEdit TextColors.definitionColor name
    M./|/ Label.make " = "
    M./|/ BuiltinEdit.make builtin myId
    M./-/ ( topLevelSchemeTypeView (builtin ^. Sugar.biType)
            & local (M.elemIdPrefix .~ elemId <> "builtinType")
        )
    where
        name = def ^. Sugar.drName
        elemId = myId & M.asElemId

make ::
    _ =>
    ExprGui.Top Sugar.Definition i o ->
    ElemId ->
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
    & local (M.elemIdPrefix .~ M.asElemId myId)

topLevelSchemeTypeView :: _ => Sugar.Scheme Name -> GuiM env i o (M.WithTextPos M.View)
topLevelSchemeTypeView scheme =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    TypeView.make (scheme ^. Sugar.schemeType)
