module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import           AST (ann)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Calc.Type (Tag)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Lens.ALens' (Texts.CodeUI Text) Text -> E.Doc
doc env lens =
    E.toDoc env
    [ has . MomentuTexts.edit
    , has . Texts.caseLabel
    , has . lens
    ]

addAltId :: Widget.Id -> Widget.Id
addAltId = (`Widget.joinId` ["add alt"])

make ::
    (Monad i, Monad o) =>
    Sugar.Case (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make (Sugar.Case mArg (Sugar.Composite alts caseTail addAlt)) pl =
    do
        caseLabel <-
            (Widget.makeFocusableView ?? headerId <&> (Align.tValue %~))
            <*> grammar (label Texts.case_)
            <&> Responsive.fromWithTextPos
        ofLabel <- grammar (label Texts.of_) <&> Responsive.fromTextView
        env <- Lens.view id
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase ->
                do
                    lambdaLabel <-
                        grammar (label Texts.lam) <&> Responsive.fromTextView
                    Options.boxSpaced
                        ?? Options.disambiguationNone
                        ?? [caseLabel, lambdaLabel, ofLabel]
                        <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg toLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression arg
                        <&> Widget.weakerEvents (toLambdaCaseEventMap env toLambdaCase)
                    mTag <-
                        Annotation.evaluationResult (arg ^. ann)
                        <&> (>>= (^? Sugar.resBody . Sugar._RInject . Sugar.riTag))
                    Options.boxSpaced
                        ?? Options.disambiguationNone
                        ?? [caseLabel, argEdit, ofLabel]
                        <&> (,) mTag
        altsGui <-
            makeAltsWidget (mActiveTag <&> (^. Sugar.tagVal)) alts addAlt altsId
            >>= case caseTail of
            Sugar.ClosedComposite actions ->
                pure . Widget.weakerEvents (closedCaseEventMap env actions)
            Sugar.OpenComposite actions rest ->
                makeOpenCase actions rest (Widget.toAnimId myId)
        let addAltEventMap =
                addAltId altsId
                & pure
                & E.keysEventMapMovesCursor (env ^. has . Config.caseAddAltKeys)
                    (doc env Texts.addAlt)
        stdWrapParentExpr pl
            <*> (Styled.addValFrame <*> (Responsive.vboxSpaced ?? [header, altsGui]))
            <&> Widget.weakerEvents addAltEventMap
    where
        myId = WidgetIds.fromExprPayload pl
        headerId = Widget.joinId myId ["header"]
        altsId = Widget.joinId myId ["alts"]

makeAltRow ::
    (Monad i, Monad o) =>
    Maybe Tag ->
    Sugar.CompositeItem (Name o) i o (ExprGui.SugarExpr i o) ->
    ExprGuiM i o (Gui Responsive.TaggedItem o)
makeAltRow mActiveTag (Sugar.CompositeItem delete tag altExpr) =
    do
        env <- Lens.view id
        addBg <- Styled.addBgColor Theme.evaluatedPathBGColor
        let itemEventMap = caseDelEventMap env delete
        altExprGui <-
            ExprGuiM.makeSubexpression altExpr <&> Widget.weakerEvents itemEventMap
        pre <-
            ( TagEdit.makeVariantTag tag
                <&> Align.tValue %~ Widget.weakerEvents itemEventMap
                <&> if mActiveTag == Just (tag ^. Sugar.tagInfo . Sugar.tagVal)
                    then addBg
                    else id
            ) /|/ grammar (label Texts.inject) /|/ Spacer.stdHSpace
        pure Responsive.TaggedItem
            { Responsive._tagPre = pre
            , Responsive._taggedItem = altExprGui
            , Responsive._tagPost = Element.empty
            }
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId altId)
    where
        altId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makeAltsWidget ::
    (Monad i, Monad o) =>
    Maybe Tag ->
    [Sugar.CompositeItem (Name o) i o (ExprGui.SugarExpr i o)] ->
    Sugar.TagSelection (Name o) i o Sugar.EntityId ->
    Widget.Id ->
    ExprGuiM i o (Gui Responsive o)
makeAltsWidget mActiveTag alts addAlt altsId =
    do
        existingAltWidgets <- traverse (makeAltRow mActiveTag) alts
        newAlts <-
            GuiState.isSubCursor ?? addAltId altsId
            <&> guard
            <&> Lens.mapped .~ makeAddAltRow addAlt (addAltId altsId)
            >>= sequenceA
        case existingAltWidgets ++ newAlts of
            [] ->
                (Widget.makeFocusableView ?? Widget.joinId altsId ["Ø"] <&> (Align.tValue %~))
                <*> grammar (label Texts.absurd)
                <&> Responsive.fromWithTextPos
            altWidgtes -> Responsive.taggedList ?? altWidgtes

makeAddAltRow ::
    (Monad i, Monad o) =>
    Sugar.TagSelection (Name o) i o Sugar.EntityId -> Widget.Id ->
    ExprGuiM i o (Gui Responsive.TaggedItem o)
makeAddAltRow addAlt myId =
    TagEdit.makeTagHoleEdit addAlt mkPickResult myId
    & Styled.withColor TextColors.caseTagColor
    <&>
    \tagHole ->
    Responsive.TaggedItem
    { Responsive._tagPre = tagHole
    , Responsive._taggedItem = Element.empty
    , Responsive._tagPost = Element.empty
    }
    where
        mkPickResult _ dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId dst
            , Menu._pickMNextEntry = WidgetIds.fromEntityId dst & Just
            }

separationBar :: TextColors -> Anim.AnimId -> Widget.R -> View
separationBar theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (theme ^. TextColors.caseTailColor)
    & Element.scale (Vector2 width 10)

makeOpenCase ::
    (Monad i, Monad o) =>
    Sugar.OpenCompositeActions o -> ExprGui.SugarExpr i o ->
    AnimId -> Gui Responsive o -> ExprGuiM i o (Gui Responsive o)
makeOpenCase actions rest animId altsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        env <- Lens.view id
        restExpr <-
            Styled.addValPadding
            <*> ExprGuiM.makeSubexpression rest
            <&> Widget.weakerEvents (openCaseEventMap env actions)
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        vbox <- Responsive.vboxWithSeparator
        vbox False
            (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            altsGui restExpr & pure

openCaseEventMap ::
    ( Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Monad o
    ) =>
    env -> Sugar.OpenCompositeActions o ->
    Gui EventMap o
openCaseEventMap env (Sugar.OpenCompositeActions close) =
    close <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env) (doc env Texts.close)

closedCaseEventMap ::
    ( Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Monad o
    ) =>
    env -> Sugar.ClosedCompositeActions o ->
    Gui EventMap o
closedCaseEventMap env (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.caseOpenKeys) (doc env Texts.open)

caseDelEventMap ::
    ( Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Monad o
    ) =>
    env -> o Sugar.EntityId -> Gui EventMap o
caseDelEventMap env delete =
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env)
    (doc env Texts.deleteAlt)

toLambdaCaseEventMap ::
    ( Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Monad o
    ) =>
    env -> o Sugar.EntityId -> Gui EventMap o
toLambdaCaseEventMap env toLamCase =
    toLamCase <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env)
    (doc env Texts.toLambdaCase)
