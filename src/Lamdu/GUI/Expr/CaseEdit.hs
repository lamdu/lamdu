module Lamdu.GUI.Expr.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu as M
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedList)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.Annotation as Annotation
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Wrap as Wrap
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
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
    ( Monad i, Monad o
    , Grid.HasTexts env, SearchMenu.HasTexts env, Has (TextEdit.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite alts punned caseTail addAlt)) =
    do
        env <- Lens.view id
        altsGui <-
            makeAltsWidget alts punned addAlt altsId
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
        header <- grammar (Label.make ".") M./|/ makeCaseLabel
        (Annotation.maybeAddAnnotationPl (pl ^. _1) <&> (Widget.widget %~)) <*>
            ( Styled.addValFrame <*>
                (Options.boxSpaced ?? Options.disambiguationNone ?? [header, altsGui]))
            <&> Widget.weakerEvents addAltEventMap
    & wrap
    where
        wrap x =
            ExprEventMap.add ExprEventMap.defaultOptions pl <*>
            (Wrap.parentDelegator myId <*> x)
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        headerId = Widget.joinId myId ["header"]
        altsId = Widget.joinId myId ["alts"]
        makeCaseLabel =
            (Widget.makeFocusableView ?? headerId <&> (M.tValue %~))
            <*> grammar (label Texts.case_)
            <&> Responsive.fromWithTextPos

makeAltRow ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    ExprGui.Body Sugar.CompositeItem i o -> GuiM env i o (TaggedItem o)
makeAltRow (Sugar.CompositeItem delete tag altExpr) =
    do
        env <- Lens.view id
        let itemEventMap = caseDelEventMap env delete
        altExprGui <-
            GuiM.makeSubexpression altExpr <&> Widget.weakerEvents itemEventMap
        pre <-
            ( TagEdit.makeVariantTag tag
                <&> M.tValue %~ Widget.weakerEvents itemEventMap
            ) M./|/ Spacer.stdHSpace
        pure TaggedItem
            { _tagPre = Just pre
            , _taggedItem = altExprGui
            , _tagPost = Nothing
            }
    & Reader.local (M.animIdPrefix .~ Widget.toAnimId altId)
    where
        altId = tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeAltsWidget ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    , Has (Texts.Definitions Text) env
    , Has (Grid.Texts Text) env
    ) =>
    [ExprGui.Body Sugar.CompositeItem i o] ->
    [Sugar.PunnedVar Name o # Annotated (ExprGui.Payload i o)] ->
    Sugar.TagChoice Name i o Sugar.EntityId ->
    Widget.Id ->
    GuiM env i o (Responsive o)
makeAltsWidget alts punned addAlt altsId =
    do
        punnedWidgets <-
            case punned of
            [] -> pure []
            _ ->
                GetVarEdit.makePunnedVars punned
                <&> (\x -> [TaggedItem Nothing x Nothing])
        existingAltWidgets <-
            traverse makeAltRow alts
            <&> (++ punnedWidgets)
        newAlts <-
            GuiState.isSubCursor ?? addAltId altsId
            <&> guard
            <&> Lens.mapped .~ makeAddAltRow addAlt (addAltId altsId)
            >>= sequenceA
        case existingAltWidgets ++ newAlts of
            [] ->
                (Widget.makeFocusableView ?? Widget.joinId altsId ["Ø"] <&> (M.tValue %~))
                <*> grammar (label Texts.absurd)
                <&> Responsive.fromWithTextPos
            altWidgtes -> taggedList ?? altWidgtes

makeAddAltRow ::
    ( Monad i, Monad o
    , Has (Texts.Name Text) env
    , Has (Texts.CodeUI Text) env
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.TagChoice Name i o Sugar.EntityId -> Widget.Id ->
    GuiM env i o (TaggedItem o)
makeAddAltRow addAlt myId =
    TagEdit.makeTagHoleEdit addAlt mkPickResult myId
    & Styled.withColor TextColors.caseTagColor
    <&>
    \tagHole ->
    TaggedItem
    { _tagPre = Just tagHole
    , _taggedItem = M.empty
    , _tagPost = Nothing
    }
    where
        mkPickResult _ dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId dst
            , Menu._pickMNextEntry = WidgetIds.fromEntityId dst & Just
            }

separationBar :: TextColors -> M.AnimId -> Widget.R -> View
separationBar theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & M.tint (theme ^. TextColors.caseTailColor)
    & M.scale (M.Vector2 width 10)

makeOpenCase ::
    ( Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Grid.HasTexts env
    ) =>
    Sugar.OpenCompositeActions o -> ExprGui.Expr Sugar.Term i o ->
    M.AnimId -> Responsive o -> GuiM env i o (Responsive o)
makeOpenCase actions rest animId altsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        env <- Lens.view id
        restExpr <-
            Styled.addValPadding
            <*> GuiM.makeSubexpression rest
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
    EventMap (o GuiState.Update)
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
    EventMap (o GuiState.Update)
closedCaseEventMap env (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.caseOpenKeys) (doc env Texts.open)

caseDelEventMap ::
    ( Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Monad o
    ) =>
    env -> o Sugar.EntityId -> EventMap (o GuiState.Update)
caseDelEventMap env delete =
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env)
    (doc env Texts.deleteAlt)
