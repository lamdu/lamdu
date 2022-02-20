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
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedListIndent)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
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
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc :: _ => env -> [Lens.ALens' (Texts.CodeUI Text) Text] -> E.Doc
doc env lens = E.toDoc env ([has . MomentuTexts.edit, has . Texts.caseLabel] <> (lens <&> (has .)))

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite alts punned caseTail)) =
    do
        env <- Lens.view id
        (addAltEventMap, altsGui) <-
            makeAltsWidget altsId alts punned
            >>= _2 %%~ case caseTail of
            Sugar.ClosedCompositeTail actions -> pure . Widget.weakerEvents (closedCaseEventMap env actions)
            Sugar.OpenCompositeTail (Sugar.OpenComposite rest) -> makeOpenCase rest (Widget.toAnimId myId)
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

makeAltRow :: _ => TaggedList.Item Name i o (ExprGui.Expr Sugar.Term i o) -> GuiM env i o [TaggedItem o]
makeAltRow item =
    do
        altExprGui <-
            GuiM.makeSubexpression (item ^. TaggedList.iValue)
            & GuiState.assignCursor
                (WidgetIds.ofTagValue altId)
                (item ^. TaggedList.iValue . annotation & WidgetIds.fromExprPayload)
        pre <-
            TagEdit.makeVariantTag (Just . TagEdit.addItemId . WidgetIds.fromEntityId) (item ^. TaggedList.iTag)
            <&> M.tValue %~ Widget.weakerEvents (item ^. TaggedList.iEventMap)
            & local (M.animIdPrefix .~ Widget.toAnimId myId)
            & local (\env -> env & has . Menu.configKeysPickOptionAndGotoNext .~ env ^. has . Config.caseAddAltKeys)
        let row =
                TaggedItem
                { _tagPre = Just pre
                , _taggedItem = M.weakerEvents (item ^. TaggedList.iEventMap) altExprGui
                , _tagPost = Nothing
                }
        makeAddAlt (item ^. TaggedList.iAddAfter) myId <&> (^.. traverse) <&> (row:)
    where
        altId = item ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance
        myId = WidgetIds.fromEntityId altId

makeAltsWidget ::
    _ =>
    Widget.Id ->
    Sugar.TaggedList Name i o (ExprGui.Expr Sugar.Term i o) ->
    [Sugar.PunnedVar Name o # Annotated (ExprGui.Payload i o)] ->
    GuiM env i o (EventMap (o GuiState.Update), Responsive o)
makeAltsWidget altsId alts punned =
    do
        punnedWidgets <-
            case punned of
            [] -> pure []
            _ ->
                GetVarEdit.makePunnedVars punned
                <&> (\x -> [TaggedItem Nothing x Nothing])
        keys <-
            traverse Lens.view TaggedList.Keys
            { TaggedList._kAdd = has . Config.caseAddAltKeys
            , TaggedList._kOrderBefore = has . Config.orderDirKeys . StdKeys.keysUp
            , TaggedList._kOrderAfter = has . Config.orderDirKeys . StdKeys.keysDown
            }
        (addAltEventMap, altItems) <- TaggedList.make (has . Texts.alternative) keys altsId altsId alts
        existingAltWidgets <- traverse makeAltRow altItems <&> concat
        prepend <- makeAddAlt (alts ^. Sugar.tlAddFirst) altsId <&> (^.. traverse)
        case prepend <> existingAltWidgets <> punnedWidgets of
            [] ->
                (Widget.makeFocusableView ?? Widget.joinId altsId ["Ø"] <&> (M.tValue %~))
                <*> grammar (label Texts.absurd)
                <&> Responsive.fromWithTextPos
            altWidgets -> taggedListIndent ?? altWidgets
            <&> (,) addAltEventMap

makeAddAlt ::
    _ =>
    i (Sugar.TagChoice Name o) -> Widget.Id -> GuiM env i o (Maybe (TaggedItem o))
makeAddAlt addField baseId =
    GuiState.isSubCursor ?? myId <&> guard
    >>= (Lens._Just . const) (GuiM.im addField >>= makeAddAltRow myId)
    where
        myId = TagEdit.addItemId baseId

makeAddAltRow ::
    _ => Widget.Id -> Sugar.TagChoice Name o -> GuiM env i o (TaggedItem o)
makeAddAltRow myId addAlt =
    TagEdit.makeTagHoleEdit mkPickResult myId addAlt
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
        mkPickResult dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.ofTagValue dst
            , Menu._pickMNextEntry = WidgetIds.ofTagValue dst & Just
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
            (separationBar (theme ^. Theme.textColors) animId <&> (\x -> vspace |---| x |---| vspace))
            altsGui restExpr & pure

closedCaseEventMap :: _ => env -> Sugar.ClosedCompositeActions o -> EventMap (o GuiState.Update)
closedCaseEventMap env (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.caseOpenKeys) (doc env [Texts.open])
