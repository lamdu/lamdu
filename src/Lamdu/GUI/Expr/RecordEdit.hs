module Lamdu.GUI.Expr.RecordEdit
    ( make
    ) where

import           AST.Knot.Ann (val)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Animation.Id (augmentId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
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
doc env lens = E.toDoc env [has . MomentuTexts.edit, has . Texts.record, has . lens]

addFieldId :: Widget.Id -> Widget.Id
addFieldId = (`Widget.joinId` ["add field"])

mkAddFieldEventMap ::
    ( MonadReader env m
    , Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Applicative o
    ) =>
    Widget.Id -> m (Gui EventMap o)
mkAddFieldEventMap myId =
    Lens.view id
    <&>
    \env ->
    addFieldId myId
    & pure
    & E.keysEventMapMovesCursor (env ^. has . Config.recordAddFieldKeys)
    (doc env Texts.addField)

addFieldWithSearchTermEventMap ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Applicative o
    ) => env -> Widget.Id -> Gui EventMap o
addFieldWithSearchTermEventMap env myId =
    E.charEventMap "Letter" (doc env Texts.addField) f
    where
        f c
            | Char.isAlpha c =
                addFieldId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit ::
    ( Monad i, Monad o
    , Has (Texts.Definitions Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Grid.HasTexts env
    ) =>
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    GuiM env i o (Gui Responsive o)
makeUnit pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (Align.tValue %~)
        addFieldEventMap <- mkAddFieldEventMap myId
        env <- Lens.view id
        stdWrap pl
            <*> ( grammar (label Texts.recordOpener)
                    /|/ grammar (label Texts.recordCloser)
                    <&> makeFocusable
                    <&> Align.tValue %~ Widget.weakerEvents
                        (addFieldEventMap <> addFieldWithSearchTermEventMap env myId)
                    <&> Responsive.fromWithTextPos
                )
    where
        myId = WidgetIds.fromExprPayload pl

make ::
    ( Monad i, Monad o
    , SearchMenu.HasTexts env
    , Grid.HasTexts env
    , Has (TextEdit.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Composite (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    GuiM env i o (Gui Responsive o)
make (Sugar.Composite [] Sugar.ClosedComposite{} addField) pl =
    -- Ignore the ClosedComposite actions - it only has the open
    -- action which is equivalent ot deletion on the unit record
    do
        isAddField <- GuiState.isSubCursor ?? addFieldId (WidgetIds.fromExprPayload pl)
        if isAddField
            then
                stdWrapParentExpr pl
                <*> (makeAddFieldRow addField pl <&> (:[]) >>= makeRecord pure)
            else makeUnit pl
make (Sugar.Composite fields recordTail addField) pl =
    do
        addFieldEventMap <- mkAddFieldEventMap (WidgetIds.fromExprPayload pl)
        tailEventMap <-
            case recordTail of
            Sugar.ClosedComposite actions ->
                closedRecordEventMap actions
            Sugar.OpenComposite actions restExpr ->
                openRecordEventMap actions restExpr
        fieldGuis <- traverse makeFieldRow fields
        isAddField <- GuiState.isSubCursor ?? addFieldId (WidgetIds.fromExprPayload pl)
        addFieldGuis <-
            if isAddField
            then makeAddFieldRow addField pl <&> (:[])
            else pure []
        env <- Lens.view id
        let goToRecordEventMap =
                WidgetIds.fromExprPayload pl & GuiState.updateCursor & pure & const
                & E.charGroup Nothing
                (E.toDoc env
                    [ has . MomentuTexts.navigation
                    , has . Texts.goToParent
                    ]) "}"
        stdWrapParentExpr pl
            <*> (makeRecord postProcess (fieldGuis ++ addFieldGuis) <&> Widget.weakerEvents goToRecordEventMap)
            <&> Widget.weakerEvents (addFieldEventMap <> tailEventMap)
    where
        postProcess =
            case recordTail of
            Sugar.OpenComposite actions restExpr ->
                makeOpenRecord actions restExpr
            _ -> pure

makeRecord ::
    ( MonadReader env m, Has Theme env, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Applicative o
    , Glue.HasTexts env, Has (Texts.Code Text) env
    ) =>
    (Gui Responsive o -> m (Gui Responsive o)) ->
    [Gui Responsive.TaggedItem o] ->
    m (Gui Responsive o)
makeRecord _ [] = error "makeRecord with no fields"
makeRecord postProcess fieldGuis =
    Styled.addValFrame <*>
    ( grammar (label Texts.recordOpener)
        /|/ (Responsive.taggedList
                <*> addPostTags fieldGuis
                >>= postProcess)
    )

addPostTags ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Has (Texts.Code Text) env, Has Dir.Layout env
    ) =>
    [Gui Responsive.TaggedItem o] -> m [Gui Responsive.TaggedItem o]
addPostTags items =
    do
        let f idx item =
                ( if idx < lastIdx
                    then label Texts.recordSep
                    else label Texts.recordCloser
                ) & grammar
                & Reader.local (Element.animIdPrefix %~ augmentId idx)
                <&> \lbl -> item & Responsive.tagPost .~ (lbl <&> Widget.fromView)
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddFieldRow ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    ) =>
    Sugar.TagReplace (Name o) i o Sugar.EntityId ->
    Sugar.Payload name i o ExprGui.Payload ->
    GuiM env i o (Gui Responsive.TaggedItem o)
makeAddFieldRow addField pl =
    TagEdit.makeTagHoleEdit addField mkPickResult tagHoleId
    & Styled.withColor TextColors.recordTagColor
    <&>
    \tagHole ->
    Responsive.TaggedItem
    { Responsive._tagPre = tagHole
    , Responsive._taggedItem = Element.empty
    , Responsive._tagPost = Element.empty
    }
    where
        tagHoleId = addFieldId (WidgetIds.fromExprPayload pl)
        mkPickResult _ dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId dst
            , Menu._pickMNextEntry = WidgetIds.fromEntityId dst & Just
            }

makeFieldRow ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.CompositeItem (Name o) i o (ExprGui.SugarExpr i o) ->
    GuiM env i o (Gui Responsive.TaggedItem o)
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        itemEventMap <- recordDelEventMap delete
        fieldGui <- GuiM.makeSubexpression fieldExpr
        pre <-
            ( TagEdit.makeRecordTag tag
                <&> Align.tValue %~ Widget.weakerEvents itemEventMap
            ) /|/ Spacer.stdHSpace
        pure Responsive.TaggedItem
            { Responsive._tagPre = pre
            , Responsive._taggedItem = Widget.weakerEvents itemEventMap fieldGui
            , Responsive._tagPost = Element.empty
            }

separationBar :: TextColors -> Anim.AnimId -> Widget.R -> View
separationBar theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (theme ^. TextColors.recordTailColor)
    & Element.scale (Vector2 width 10)

makeOpenRecord ::
    (Monad i, Monad o, Glue.HasTexts env, Has (Texts.CodeUI Text) env) =>
    Sugar.OpenCompositeActions o -> ExprGui.SugarExpr i o ->
    Gui Responsive o -> GuiM env i o (Gui Responsive o)
makeOpenRecord (Sugar.OpenCompositeActions close) rest fieldsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        env <- Lens.view id
        let restEventMap =
                close <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (doc env Texts.close)
        restExpr <-
            Styled.addValPadding <*> GuiM.makeSubexpression rest
            <&> Widget.weakerEvents restEventMap
        animId <- Lens.view Element.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? fieldsGui ?? restExpr

openRecordEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    Sugar.OpenCompositeActions o ->
    Sugar.Expression name i o a ->
    m (Gui EventMap o)
openRecordEventMap (Sugar.OpenCompositeActions close) restExpr
    | isHole restExpr =
        Lens.view id
        <&>
        \env ->
        close <&> WidgetIds.fromEntityId
        & E.keysEventMapMovesCursor (env ^. has . Config.recordCloseKeys)
        (doc env Texts.close)
    | otherwise = pure mempty
    where
        isHole = Lens.has (val . Sugar._BodyHole)

closedRecordEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    Sugar.ClosedCompositeActions o -> m (Gui EventMap o)
closedRecordEventMap (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.recordOpenKeys)
    (doc env Texts.open)

recordDelEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    o Sugar.EntityId -> m (Gui EventMap o)
recordDelEventMap delete =
    Lens.view id
    <&>
    \env ->
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env) (doc env Texts.deleteField)
