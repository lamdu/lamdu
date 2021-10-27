module Lamdu.GUI.Expr.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
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
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc :: _ => env -> Lens.ALens' (Texts.CodeUI Text) Text -> E.Doc
doc env lens =
    E.toDoc env
    [ has . MomentuTexts.edit
    , has . Texts.caseLabel
    , has . lens
    ]

addAltId :: Widget.Id -> Widget.Id
addAltId = (`Widget.joinId` ["add alt"])

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite alts punned caseTail)) =
    do
        env <- Lens.view id
        (addAltEventMap, altsGui) <-
            makeAltsWidget altsId alts punned
            >>= _2 %%~ case caseTail of
            Sugar.ClosedComposite actions -> pure . Widget.weakerEvents (closedCaseEventMap env actions)
            Sugar.OpenComposite rest -> makeOpenCase rest (Widget.toAnimId myId)
        header <- grammar (Label.make ".") M./|/ makeCaseLabel
        Styled.addValFrame <*>
            (Options.boxSpaced ?? Options.disambiguationNone ?? [header, altsGui])
            & stdWrapParentExpr pl
            <&> Widget.weakerEvents addAltEventMap
    where
        myId = WidgetIds.fromExprPayload pl
        headerId = Widget.joinId myId ["header"]
        altsId = Widget.joinId myId ["alts"]
        makeCaseLabel =
            (Widget.makeFocusableView ?? headerId <&> (M.tValue %~))
            <*> grammar (label Texts.case_)
            <&> Responsive.fromWithTextPos

makeAltRow :: _ => ExprGui.Body (Sugar.TaggedItem Sugar.Term) i o -> GuiM env i o (TaggedItem o)
makeAltRow (Sugar.TaggedItem tag delete _addAfter altExpr) =
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
    & local (M.animIdPrefix .~ Widget.toAnimId altId)
    where
        altId = tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeAltsWidget ::
    _ =>
    Widget.Id ->
    ExprGui.Body (Sugar.TaggedList Sugar.Term) i o ->
    [Sugar.PunnedVar Name o # Annotated (ExprGui.Payload i o)] ->
    GuiM env i o (EventMap _, Responsive o)
makeAltsWidget altsId (Sugar.TaggedList addAlt alts) punned =
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
        env <- Lens.view id
        let addAltEventMap =
                GuiState.updateCursor dst
                & GuiState.uWidgetStateUpdates . Lens.at dst ?~ mempty
                & pure
                & E.keyPresses (env ^. has . Config.caseAddAltKeys)
                    (doc env Texts.addAlt)
                where
                    dst = addAltId altsId
        case existingAltWidgets ++ newAlts of
            [] ->
                (Widget.makeFocusableView ?? Widget.joinId altsId ["Ø"] <&> (M.tValue %~))
                <*> grammar (label Texts.absurd)
                <&> Responsive.fromWithTextPos
            altWidgtes -> taggedList ?? altWidgtes
            <&> (,) addAltEventMap

makeAddAltRow ::
    _ => Sugar.TagChoice Name i o Sugar.EntityId -> Widget.Id -> GuiM env i o (TaggedItem o)
makeAddAltRow addAlt myId =
    TagEdit.makeTagHoleEdit addAlt mkPickResult myId
    & Styled.withColor TextColors.caseTagColor
    & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [M.noMods M.Key'Space])
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
    _ => ExprGui.Expr Sugar.Term i o -> M.AnimId -> Responsive o -> GuiM env i o (Responsive o)
makeOpenCase rest animId altsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        restExpr <-
            Styled.addValPadding
            <*> GuiM.makeSubexpression rest
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        vbox <- Responsive.vboxWithSeparator
        vbox False
            (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            altsGui restExpr & pure

closedCaseEventMap :: _ => env -> Sugar.ClosedCompositeActions o -> EventMap (o GuiState.Update)
closedCaseEventMap env (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.caseOpenKeys) (doc env Texts.open)

caseDelEventMap :: _ => env -> o Sugar.EntityId -> EventMap (o GuiState.Update)
caseDelEventMap env delete =
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env)
    (doc env Texts.deleteAlt)
