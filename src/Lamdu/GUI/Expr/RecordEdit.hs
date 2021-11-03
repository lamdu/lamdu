module Lamdu.GUI.Expr.RecordEdit
    ( make, makeEmpty
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedList, tagPost)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
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
import           Lamdu.GUI.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified GUI.Momentu.State as M

doc :: _ => env -> Lens.ALens' (Texts.CodeUI Text) Text -> E.Doc
doc env lens = E.toDoc env [has . MomentuTexts.edit, has . Texts.record, has . lens]

addFieldId :: Widget.Id -> Widget.Id
addFieldId = (`Widget.joinId` ["add field"])

mkAddFieldEventMap :: _ => Widget.Id -> m (EventMap (o GuiState.Update))
mkAddFieldEventMap myId =
    Lens.view id
    <&>
    \env ->
    addFieldId myId
    & pure
    & E.keysEventMapMovesCursor (env ^. has . Config.recordAddFieldKeys)
    (doc env Texts.addField)

addFieldWithSearchTermEventMap :: _ => env -> Widget.Id -> EventMap (o GuiState.Update)
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

makeUnit :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o)
makeUnit pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (M.tValue %~)
        addFieldEventMap <- mkAddFieldEventMap myId
        env <- Lens.view id
        grammar (label Texts.recordOpener)
            M./|/ grammar (label Texts.recordCloser)
            <&> makeFocusable
            <&> M.tValue %~ Widget.weakerEvents
                (addFieldEventMap <> addFieldWithSearchTermEventMap env myId)
            <&> Responsive.fromWithTextPos
            & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

makeEmpty ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.TagChoice Name i o) ->
    GuiM env i o (Responsive o)
makeEmpty (Ann (Const pl) (Const addField)) =
    do
        isAddField <- GuiState.isSubCursor ?? addFieldId (WidgetIds.fromExprPayload pl)
        if isAddField
            then
                makeAddFieldRow addField pl <&> (:[]) >>= makeRecord pure
                & stdWrapParentExpr pl
            else makeUnit pl

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite (Sugar.TaggedList addField Nothing) [] Sugar.ClosedComposite{})) =
    -- Ignore the ClosedComposite actions - it only has the open
    -- action which is equivalent ot deletion on the unit record
    makeEmpty (Ann (Const pl) (Const addField))
make (Ann (Const pl) (Sugar.Composite (Sugar.TaggedList addField mTlBody) punned recordTail)) =
    do
        addFieldEventMap <- mkAddFieldEventMap (WidgetIds.fromExprPayload pl)
        tailEventMap <-
            case recordTail of
            Sugar.ClosedComposite actions -> closedRecordEventMap actions
            Sugar.OpenComposite{} -> pure mempty
        punnedGuis <-
            case punned of
            [] -> pure []
            _ ->
                GetVarEdit.makePunnedVars punned
                <&> (\x -> [TaggedItem Nothing x Nothing])
        fieldGuis <- traverse makeFieldRow fields <&> (++ punnedGuis)
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
        makeRecord postProcess (fieldGuis ++ addFieldGuis)
            <&> Widget.weakerEvents goToRecordEventMap
            <&> Widget.weakerEvents (addFieldEventMap <> tailEventMap)
            & stdWrapParentExpr pl
    where
        fields = mTlBody ^.. Lens._Just . SugarLens.taggedListItems
        postProcess =
            case recordTail of
            Sugar.OpenComposite restExpr -> makeOpenRecord restExpr
            _ -> pure

makeRecord :: _ => (Responsive o -> m (Responsive o)) -> [TaggedItem o] -> m (Responsive o)
makeRecord _ [] = error "makeRecord with no fields"
makeRecord postProcess fieldGuis =
    Styled.addValFrame <*>
    ( grammar (label Texts.recordOpener)
        M./|/ (taggedList
                <*> addPostTags fieldGuis
                >>= postProcess)
    )

addPostTags :: _ => [TaggedItem o] -> m [TaggedItem o]
addPostTags items =
    do
        let f idx item =
                label t & grammar
                & Element.locallyAugmented idx
                <&> \lbl -> item & tagPost ?~ (lbl <&> Widget.fromView)
                where
                    t :: Lens' (Texts.Code a) a
                    t | idx < lastIdx = Texts.recordSep
                        | otherwise = Texts.recordCloser
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddFieldRow ::
    _ =>
    Sugar.TagChoice Name i o ->
    Sugar.Payload v o ->
    GuiM env i o (TaggedItem o)
makeAddFieldRow addField pl =
    TagEdit.makeTagHoleEdit addField mkPickResult tagHoleId
    & Styled.withColor TextColors.recordTagColor
    & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [M.noMods M.Key'Space])
    <&>
    \tagHole ->
    TaggedItem
    { _tagPre = Just tagHole
    , _taggedItem = M.empty
    , _tagPost = Just M.empty
    }
    where
        tagHoleId = addFieldId (WidgetIds.fromExprPayload pl)
        mkPickResult dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.ofTagValue dst
            , Menu._pickMNextEntry = WidgetIds.ofTagValue dst & Just
            }

makeFieldRow :: _ => Sugar.TaggedItem Name i o (ExprGui.Expr Sugar.Term i o) -> GuiM env i o (TaggedItem o)
makeFieldRow (Sugar.TaggedItem tag delete _addAfter fieldExpr) =
    do
        itemEventMap <- recordDelEventMap delete
        fieldGui <- GuiM.makeSubexpression fieldExpr
        pre <-
            ( TagEdit.makeRecordTag tag
                <&> M.tValue %~ Widget.weakerEvents itemEventMap
            ) M./|/ Spacer.stdHSpace
        pure TaggedItem
            { _tagPre = Just pre
            , _taggedItem = Widget.weakerEvents itemEventMap fieldGui
            , _tagPost = Just M.empty
            }
    & M.assignCursor
        (WidgetIds.ofTagValue (tag ^. Sugar.tagRefTag . Sugar.tagInstance))
        (fieldExpr ^. annotation & WidgetIds.fromExprPayload)

separationBar :: TextColors -> M.AnimId -> Widget.R -> M.View
separationBar theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & M.tint (theme ^. TextColors.recordTailColor)
    & M.scale (M.Vector2 width 10)

makeOpenRecord :: _ => ExprGui.Expr Sugar.Term i o -> Responsive o -> GuiM env i o (Responsive o)
makeOpenRecord rest fieldsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        restExpr <-
            Styled.addValPadding <*> GuiM.makeSubexpression rest
        animId <- Lens.view M.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? fieldsGui ?? restExpr

closedRecordEventMap :: _ => Sugar.ClosedCompositeActions o -> m (EventMap (o GuiState.Update))
closedRecordEventMap (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.recordOpenKeys)
    (doc env Texts.open)

recordDelEventMap :: _ => o Sugar.EntityId -> m (EventMap (o GuiState.Update))
recordDelEventMap delete =
    Lens.view id
    <&>
    \env ->
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env) (doc env Texts.deleteField)
