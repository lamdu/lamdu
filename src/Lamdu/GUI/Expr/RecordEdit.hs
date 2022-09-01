module Lamdu.GUI.Expr.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import           GUI.Momentu (Responsive, EventMap, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedListIndent, tagPre, tagPost)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
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
import           Lamdu.GUI.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude
import qualified GUI.Momentu.State as M

doc :: _ => env -> [Lens.ALens' (Texts.CodeUI Text) Text] -> E.Doc
doc env lens =
    E.toDoc env ([has . MomentuTexts.edit, has . Texts.record] <> (lens <&> (has .)))

addFieldWithSearchTermEventMap :: _ => env -> Widget.Id -> EventMap (o Update)
addFieldWithSearchTermEventMap env myId =
    E.charEventMap "Letter" (doc env [Texts.field, Texts.add]) f
    where
        f c
            | Char.isAlpha c =
                TagEdit.addItemId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o)
makeUnit pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (M.tValue %~)
        env <- Lens.view id
        let addFieldEventMap =
                E.keysEventMapMovesCursor (env ^. has . Config.compositeAddItemKeys)
                (doc env [Texts.field, Texts.add])
                (pure (TagEdit.addItemId myId))
        grammar (label Texts.recordOpener)
            M./|/ grammar (label Texts.recordCloser)
            <&> makeFocusable
            <&> M.tValue %~ Widget.weakerEvents
                (addFieldEventMap <> addFieldWithSearchTermEventMap env myId)
            <&> Responsive.fromWithTextPos
            & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

makeAddField ::
    _ =>
    i (Sugar.TagChoice Name o) -> Widget.Id -> GuiM env i o (Maybe (TaggedItem o))
makeAddField addField baseId =
    GuiState.isSubCursor ?? myId <&> guard
    >>= (Lens._Just . const) (GuiM.im addField >>= makeAddFieldRow myId)
    where
        myId = TagEdit.addItemId baseId

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite (Sugar.TaggedList addField mTlBody) punned recordTail)) =
    do
        tailEventMap <-
            case recordTail of
            Sugar.ClosedCompositeTail actions -> closedRecordEventMap actions
            Sugar.OpenCompositeTail{} -> pure mempty
        env <- Lens.view id
        let goToRecordEventMap =
                WidgetIds.fromExprPayload pl & GuiState.updateCursor & pure & const
                & E.charGroup Nothing
                (E.toDoc env
                    [ has . MomentuTexts.navigation
                    , has . Texts.goToParent
                    ]) "}"
        keys <-
            traverse Lens.view TaggedList.Keys
            { TaggedList._kAdd = has . Config.compositeAddItemKeys
            , TaggedList._kOrderBefore = has . Config.orderDirKeys . StdKeys.keysUp
            , TaggedList._kOrderAfter = has . Config.orderDirKeys . StdKeys.keysDown
            }
        let prependEventMap = addFieldWithSearchTermEventMap env myId
        items <- mconcat
            [ makeAddField addField myId <&> (^.. traverse)
            , foldMap (TaggedList.makeBody (has . Texts.field) keys (pure myId) (pure myId)) mTlBody
                >>= traverse makeFieldRow
                <&> concat
                <&> Lens.ix 0 . tagPre . Lens._Just . M.tValue %~ M.weakerEvents prependEventMap
            , case punned of
                [] -> pure []
                _ ->
                    M.weakerEvents
                    <$> TaggedList.addNextEventMap
                        (has . Texts.field) (keys ^. TaggedList.kAdd) (pure punAddId)
                    <*> GetVarEdit.makePunnedVars punned
                    <&> (:[]) . (TaggedItem Nothing ?? Nothing)
                ]
        case items of
            [] -> makeUnit pl
            _ ->
                Styled.addValFrame <*>
                ( grammar (label Texts.recordOpener)
                    M./|/ (taggedListIndent <*> addPostTags items >>= postProcess)
                ) <&> Widget.weakerEvents (goToRecordEventMap <> tailEventMap)
                & stdWrapParentExpr pl
    & (foldl assignPunned ?? punned)
    where
        assignPunned w p =
            M.assignCursorPrefix
            (WidgetIds.fromEntityId (p ^. Sugar.pvTagEntityId)) (Widget.joinId punAddId) w
        myId = WidgetIds.fromExprPayload pl
        punAddId =
            mTlBody >>= Lens.lastOf (SugarLens.taggedListBodyItems . Sugar.tiTag)
            & maybe myId TaggedList.itemId
        postProcess =
            case recordTail of
            Sugar.OpenCompositeTail restExpr -> makeOpenRecord restExpr
            _ -> pure

addPostTags :: _ => [TaggedItem o] -> m [TaggedItem o]
addPostTags items =
    do
        let f idx item =
                label (if isComma then Texts.recordSep else Texts.recordCloser)
                & grammar
                & (if isComma then Element.locallyAugmented idx else id)
                <&> \lbl -> item & tagPost ?~ (lbl <&> Widget.fromView)
                where
                    isComma = idx < lastIdx
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddFieldRow ::
    _ =>
    Widget.Id ->
    Sugar.TagChoice Name o ->
    GuiM env i o (TaggedItem o)
makeAddFieldRow tagHoleId addField =
    TagEdit.makeTagHoleEdit mkPickResult tagHoleId addField
    & Styled.withColor TextColors.recordTagColor
    & setPickAndAddNextKeys
    <&>
    \tagHole ->
    TaggedItem
    { _tagPre = Nothing
    , _taggedItem = Responsive.fromWithTextPos tagHole
    , _tagPost = Just M.empty
    }
    where
        mkPickResult dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.ofTagValue dst
            , Menu._pickMNextEntry = WidgetIds.fromEntityId dst & TagEdit.addItemId & Just
            }

setPickAndAddNextKeys :: _ => GuiM env i o a -> GuiM env i o a
setPickAndAddNextKeys =
    local
    (\env ->
        env
        & has . Menu.configKeysPickOptionAndGotoNext .~ (env ^. has . Config.compositeAddItemKeys :: [M.ModKey])
        & has . Menu.configKeysPickOption <>~
            env ^. has . Menu.configKeysPickOptionAndGotoNext <> [M.noMods M.Key'Space]
    )

makeFieldRow ::
    _ =>
    TaggedList.Item Name i o (ExprGui.Expr Sugar.Term i o) ->
    GuiM env i o [TaggedItem o]
makeFieldRow item =
    do
        fieldGui <-
            GuiM.makeSubexpression (item ^. TaggedList.iValue)
            & M.assignCursor (WidgetIds.ofTagValue fieldId)
                (item ^. TaggedList.iValue . annotation & WidgetIds.fromExprPayload)
        pre <-
            TagEdit.makeColoredTag TextColors.recordTagColor
            (Just . TagEdit.addItemId . WidgetIds.fromEntityId) (item ^. TaggedList.iTag)
            <&> M.tValue %~ Widget.weakerEvents (item ^. TaggedList.iEventMap)
            & setPickAndAddNextKeys
        let row =
                TaggedItem
                { _tagPre = Just pre
                , _taggedItem = M.weakerEvents (item ^. TaggedList.iEventMap) fieldGui
                , _tagPost = Just M.empty
                }
        makeAddField (item ^. TaggedList.iAddAfter) myId <&> (^.. traverse) <&> (row:)
    where
        fieldId = item ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance
        myId = WidgetIds.fromEntityId fieldId

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
        restExpr <- Styled.addValPadding <*> GuiM.makeSubexpression rest
        animId <- Lens.view M.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? fieldsGui ?? restExpr

closedRecordEventMap :: _ => Sugar.ClosedCompositeActions o -> m (EventMap (o Update))
closedRecordEventMap (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.compositeOpenKeys)
    (doc env [Texts.open])
