{-# LANGUAGE FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
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
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc :: E.Subtitle -> E.Doc
doc text = E.Doc ["Edit", "Record", text]

addFieldId :: Widget.Id -> Widget.Id
addFieldId = (`Widget.joinId` ["add field"])

mkAddFieldEventMap ::
    (MonadReader env m, HasConfig env, Applicative o) =>
    Widget.Id -> m (Gui EventMap o)
mkAddFieldEventMap myId =
    Lens.view (Config.config . Config.recordAddFieldKeys)
    <&>
    \keys ->
    addFieldId myId
    & pure
    & E.keysEventMapMovesCursor keys (doc "Add Field")

addFieldWithSearchTermEventMap :: Applicative o => Widget.Id -> Gui EventMap o
addFieldWithSearchTermEventMap myId =
    E.charEventMap "Letter" (doc "Add Field") f
    where
        f c
            | Char.isAlpha c =
                addFieldId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit ::
    (Monad i, Monad o) =>
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
makeUnit pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (Align.tValue %~)
        addFieldEventMap <- mkAddFieldEventMap myId
        stdWrap pl
            <*> ( grammar (label (Texts.code . Texts.recordOpener))
                    /|/ grammar (label (Texts.code . Texts.recordCloser))
                    <&> makeFocusable
                    <&> Align.tValue %~ Widget.weakerEvents
                        (addFieldEventMap <> addFieldWithSearchTermEventMap myId)
                    <&> Responsive.fromWithTextPos
                )
    where
        myId = WidgetIds.fromExprPayload pl

make ::
    (Monad i, Monad o) =>
    Sugar.Composite (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
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
        stdWrapParentExpr pl
            <*> (makeRecord postProcess (fieldGuis ++ addFieldGuis) <&> Widget.weakerEvents goToRecordEventMap)
            <&> Widget.weakerEvents (addFieldEventMap <> tailEventMap)
    where
        postProcess =
            case recordTail of
            Sugar.OpenComposite actions restExpr ->
                makeOpenRecord actions restExpr
            _ -> pure
        goToRecordEventMap =
            WidgetIds.fromExprPayload pl & GuiState.updateCursor & pure & const
            & E.charGroup Nothing (E.Doc ["Navigation", "Go to parent"]) "}"

makeRecord ::
    ( MonadReader env m, Theme.HasTheme env, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Texts.HasLanguage env, Applicative o
    ) =>
    (Gui Responsive o -> m (Gui Responsive o)) ->
    [Gui Responsive.TaggedItem o] ->
    m (Gui Responsive o)
makeRecord _ [] = error "makeRecord with no fields"
makeRecord postProcess fieldGuis =
    Styled.addValFrame <*>
    ( grammar (label (Texts.code . Texts.recordOpener))
        /|/ (Responsive.taggedList
                <*> addPostTags fieldGuis
                >>= postProcess)
    )

addPostTags ::
    ( MonadReader env m, Theme.HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env, Texts.HasLanguage env
    ) =>
    [Gui Responsive.TaggedItem o] -> m [Gui Responsive.TaggedItem o]
addPostTags items =
    do
        let f idx item =
                ( if idx < lastIdx
                    then label (Texts.code . Texts.recordSep)
                    else label (Texts.code . Texts.recordCloser)
                ) & grammar
                & Reader.local (Element.animIdPrefix %~ augmentId idx)
                <&> \lbl -> item & Responsive.tagPost .~ (lbl <&> Widget.fromView)
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddFieldRow ::
    (Monad i, Monad o) =>
    Sugar.TagSelection (Name o) i o Sugar.EntityId ->
    Sugar.Payload name i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive.TaggedItem o)
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
    (Monad i, Monad o) =>
    Sugar.CompositeItem (Name o) i o (ExprGui.SugarExpr i o) ->
    ExprGuiM i o (Gui Responsive.TaggedItem o)
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        itemEventMap <- recordDelEventMap delete
        fieldGui <- ExprGuiM.makeSubexpression fieldExpr
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
    (Monad i, Monad o) =>
    Sugar.OpenCompositeActions o -> ExprGui.SugarExpr i o ->
    Gui Responsive o -> ExprGuiM i o (Gui Responsive o)
makeOpenRecord (Sugar.OpenCompositeActions close) rest fieldsGui =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        config <- Lens.view Config.config
        let restEventMap =
                close <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Close")
        restExpr <-
            Styled.addValPadding <*> ExprGuiM.makeSubexpression rest
            <&> Widget.weakerEvents restEventMap
        animId <- Lens.view Element.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? fieldsGui ?? restExpr

openRecordEventMap ::
    (MonadReader env m, HasConfig env, Functor o) =>
    Sugar.OpenCompositeActions o ->
    Sugar.Expression name i o a ->
    m (Gui EventMap o)
openRecordEventMap (Sugar.OpenCompositeActions close) restExpr
    | isHole restExpr =
        Lens.view (Config.config . Config.recordCloseKeys)
        <&>
        \keys ->
        close <&> WidgetIds.fromEntityId
        & E.keysEventMapMovesCursor keys (doc "Close")
    | otherwise = pure mempty
    where
        isHole = Lens.has (val . Sugar._BodyHole)

closedRecordEventMap ::
    (MonadReader env m, HasConfig env, Functor o) =>
    Sugar.ClosedCompositeActions o -> m (Gui EventMap o)
closedRecordEventMap (Sugar.ClosedCompositeActions open) =
    Lens.view (Config.config . Config.recordOpenKeys)
    <&>
    \keys ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor keys (doc "Open")

recordDelEventMap ::
    (MonadReader env m, HasConfig env, Functor o) =>
    o Sugar.EntityId -> m (Gui EventMap o)
recordDelEventMap delete =
    Lens.view Config.config <&> Config.delKeys
    <&>
    \keys ->
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor keys (doc "Delete Field")
