{-# LANGUAGE TemplateHaskell, PolymorphicComponents #-}
module Lamdu.GUI.Expr.CompositeEdit
    ( Config(..), name, itemName, opener, closer
    , make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import qualified Data.Char as Char
import qualified Data.Text as Text
import           GUI.Momentu (Responsive, EventMap, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedListIndent, tagPre, tagPost)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.State as M
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import           Hyper (HFoldable(..), hflipped)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme.TextColors (TextColors)
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

data Config = Config
    { _name :: !Text -- "record" | "case"
    , _itemName :: !Text -- "field" | "alternative"
    , _opener :: forall a. Lens.ALens' (Texts.Code a) a -- "{" | "["
    , _closer :: forall a. Lens.ALens' (Texts.Code a) a -- "}" | "]"
    , _tagColor :: !(Lens.ALens' TextColors M.Color)
    }
Lens.makeLenses ''Config

editDoc :: Has (MomentuTexts.Texts Text) env => env -> [Text] -> E.Doc
editDoc env texts =
    env ^. has . MomentuTexts.edit : texts & E.Doc

itemDocPrefix :: Config -> [Text]
itemDocPrefix conf = [conf ^. name, conf ^. itemName]

addItemWithSearchTermEventMap :: _ => Config -> env -> ElemId -> EventMap (o Update)
addItemWithSearchTermEventMap conf env myId =
    E.charEventMap "Letter" (editDoc env (itemDocPrefix conf ++ [env ^. has . Texts.add])) f
    where
        f c
            | Char.isAlpha c =
                TagEdit.addItemId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit ::
    _ =>
    Maybe (Responsive o -> Responsive o) -> ElemId -> Config -> ExprGui.Payload i o -> GuiM env i o (Responsive o)
makeUnit prependKeywords myId conf pl =
    do
        env <- Lens.view id
        let addItemEventMap =
                E.keysEventMapMovesCursor (env ^. has . Config.compositeAddItemKeys)
                (editDoc env (itemDocPrefix conf ++ [env ^. has . Texts.add]))
                (pure (TagEdit.addItemId myId))
        grammar (label (conf ^. opener))
            M./|/ grammar (label (conf ^. closer))
            >>= M.tValue (Widget.makeFocusableView myId)
            <&> M.tValue %~ Widget.weakerEvents
                (addItemEventMap <> addItemWithSearchTermEventMap conf env myId)
            <&> Responsive.fromWithTextPos
            & case prependKeywords of
            Nothing -> stdWrap pl
            Just prepend -> stdWrapParentExpr pl . fmap prepend

makeAddItem ::
    _ =>
    Config -> i (Sugar.TagChoice Name o) ->
    o ElemId -> o ElemId ->
    ElemId -> GuiM env i o (Maybe (TaggedItem o))
makeAddItem conf addItem prevId nextId baseId =
    GuiState.isSubCursor myId <&> guard
    >>= (Lens._Just . const) (GuiM.im addItem >>= makeAddItemRow conf prevId nextId myId)
    where
        myId = TagEdit.addItemId baseId

make ::
    (HasCallStack, _) =>
    Maybe (Responsive o -> Responsive o) ->
    ElemId -> Config -> ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make prependKeywords myId conf (Ann (Const pl) (Sugar.Composite tl punned compositeTail)) =
    do
        tailEventMap <-
            case compositeTail of
            Sugar.ClosedCompositeTail actions -> closedCompositeEventMap conf actions
            Sugar.OpenCompositeTail{} -> pure mempty
        env <- Lens.view id
        let goToParentEventMap =
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
        let prependEventMap = addItemWithSearchTermEventMap conf env myId
        (addNextEventMap, body) <- TaggedList.make (itemDocPrefix conf) keys (pure myId) (pure myId) tl
        let prevs =
                ( drop 1 body
                    <&> (^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance)
                    <&> Just . pure . WidgetIds.fromEntityId
                ) <> [Nothing]
        items <-
            mconcat
            [ makeAddItem conf (tl ^. Sugar.tlAddFirst) (pure myId) (pure myId) myId <&> (^.. traverse)
            , zipWithM (makeItemRow conf) prevs body
                <&> concat
                <&> Lens.ix 0 . tagPre . Lens._Just . M.tValue %~ M.weakerEvents prependEventMap
            , case punned of
                [] -> pure []
                _ ->
                    GetVarEdit.makePunnedVars punned
                    <&> M.weakerEvents addNextEventMap
                    <&> (:[]) . (TaggedItem Nothing ?? Nothing)
                ]
        case items of
            [] -> makeUnit prependKeywords myId conf pl
            _ ->
                ( grammar (label (conf ^. opener))
                    M./|/ (addPostTags conf items >>= postProcess >>= taggedListIndent)
                ) >>= Styled.addValFrame
                <&> Widget.weakerEvents (goToParentEventMap <> tailEventMap)
                <&> fromMaybe id prependKeywords
                & stdWrapParentExpr pl
        & (foldl assignPunned ?? punned)
    where
        assignPunned w p =
            M.assignCursorPrefix
            (WidgetIds.fromEntityId (p ^. Sugar.pvTagEntityId)) (punAddId <>) w
        punAddId =
            tl ^. Sugar.tlItems
            >>= Lens.lastOf (SugarLens.taggedListBodyItems . Sugar.tiTag)
            & maybe myId TaggedList.itemId
        postProcess items =
            case compositeTail of
            Sugar.ClosedCompositeTail{} -> pure items
            Sugar.OpenCompositeTail restExpr ->
                do
                    rest <- GuiM.makeSubexpression restExpr
                    ext <- label Texts.compositeExtendTail & grammar
                    (items & Lens._last . tagPost .~ Nothing) <>
                        [ TaggedItem
                            { _tagPre = Just (ext <&> Widget.fromView)
                            , _taggedItem = rest
                            , _tagPost = items ^? Lens._last . tagPost . Lens._Just
                            }
                        ] & pure

addPostTags :: _ => Config -> [TaggedItem o] -> m [TaggedItem o]
addPostTags conf items =
    do
        let f idx item =
                label (if isComma then Texts.compositeSeparator else conf ^. closer)
                & grammar
                & (if isComma then Element.locallyAugmented idx else id)
                <&> \lbl -> item & tagPost ?~ (lbl <&> Widget.fromView)
                where
                    isComma = idx < lastIdx
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddItemRow ::
    _ =>
    Config -> o ElemId -> o ElemId -> ElemId ->
    Sugar.TagChoice Name o ->
    GuiM env i o (TaggedItem o)
makeAddItemRow conf prevId nextId tagHoleId addItem =
    do
        tagHole <-
            TagEdit.makeTagHoleEdit mkPickResult tagHoleId addItem
            & Styled.withColor (conf ^. tagColor)
            & setPickAndAddNextKeys
        delEvent <- TaggedList.delEventMap (itemDocPrefix conf) (pure ()) prevId nextId
        pure TaggedItem
            { _tagPre = Nothing
            , _taggedItem =
                tagHole
                <&> M.weakerEvents delEvent
                & Responsive.fromWithTextPos
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
            env ^. has . Menu.configKeysPickOptionAndGotoNext <>
            [ M.noMods M.Key'Space
            , case env ^. has of
                Dir.LeftToRight -> M.Key'Right
                Dir.RightToLeft -> M.Key'Left
                & M.noMods
            ]
    )

makeItemRow ::
    _ =>
    Config -> Maybe (o ElemId) -> TaggedList.Item Name i o (ExprGui.Expr Sugar.Term i o) ->
    GuiM env i o [TaggedItem o]
makeItemRow conf mNextId item =
    do
        itemGui <-
            GuiM.makeSubexpression (item ^. TaggedList.iValue)
            & M.assignCursor (WidgetIds.ofTagValue itemId)
                (item ^. TaggedList.iValue . annotation & WidgetIds.fromExprPayload)
        pre <-
            TagEdit.makeColoredTag (conf ^. tagColor)
            (Just . TagEdit.addItemId . WidgetIds.fromEntityId) (item ^. TaggedList.iTag)
            <&> M.tValue %~ Widget.weakerEvents (item ^. TaggedList.iEventMap)
            & setPickAndAddNextKeys
        let row =
                TaggedItem
                { _tagPre = Just pre
                , _taggedItem = M.weakerEvents (item ^. TaggedList.iEventMap) itemGui
                , _tagPost = Just M.empty
                }
        makeAddItem conf (item ^. TaggedList.iAddAfter)
            (pure lastSubExprId) (fromMaybe (pure lastSubExprId) mNextId) myId
            <&> (^.. traverse) <&> (row:)
    where
        itemId = item ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance
        myId = WidgetIds.fromEntityId itemId
        lastSubExprId =
            hfoldMap (const ((:[]) . getConst)) (item ^. TaggedList.iValue . hflipped)
            ^? Lens._last
            & fromMaybe (error "no subexpressions for expr")
            & WidgetIds.fromExprPayload

closedCompositeEventMap :: _ => Config -> Sugar.ClosedCompositeActions o -> m (EventMap (o Update))
closedCompositeEventMap conf (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.compositeOpenKeys)
    (editDoc env [conf ^. name, env ^. has . Texts.open])
