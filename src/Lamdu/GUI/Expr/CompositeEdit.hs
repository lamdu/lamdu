{-# LANGUAGE TemplateHaskell, PolymorphicComponents #-}
module Lamdu.GUI.Expr.CompositeEdit
    ( Config(..), name, itemName, opener, closer
    , make
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
import qualified GUI.Momentu.State as M
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.StdKeys as StdKeys
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
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
    { _name :: Lens.ALens' (Texts.CodeUI Text) Text -- "record" | "case"
    , _itemName :: Lens.ALens' (Texts.CodeUI Text) Text -- "field" | "alternative"
    , _opener :: forall a. Lens.ALens' (Texts.Code a) a -- "{" | "["
    , _closer :: forall a. Lens.ALens' (Texts.Code a) a -- "}" | "]"
    , _tailColor :: Lens.ALens' TextColors M.Color
    , _tagColor :: Lens.ALens' TextColors M.Color
    }
Lens.makeLenses ''Config

doc :: _ => env -> [Lens.ALens' (Texts.CodeUI Text) Text] -> E.Doc
doc env lens =
    E.toDoc env ([has . MomentuTexts.edit] <> (lens <&> (has .)))

itemDocPrefix :: Config -> [Lens.ALens' (Texts.CodeUI Text) Text]
itemDocPrefix conf = [conf ^. name, conf ^. itemName]

addItemWithSearchTermEventMap :: _ => Config -> env -> Widget.Id -> EventMap (o Update)
addItemWithSearchTermEventMap conf env myId =
    E.charEventMap "Letter" (doc env (itemDocPrefix conf ++ [Texts.add])) f
    where
        f c
            | Char.isAlpha c =
                TagEdit.addItemId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit :: _ => Config -> ExprGui.Payload i o -> GuiM env i o (Responsive o)
makeUnit conf pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (M.tValue %~)
        env <- Lens.view id
        let addItemEventMap =
                E.keysEventMapMovesCursor (env ^. has . Config.compositeAddItemKeys)
                (doc env (itemDocPrefix conf ++ [Texts.add]))
                (pure (TagEdit.addItemId myId))
        grammar (label (conf ^. opener))
            M./|/ grammar (label (conf ^. closer))
            <&> makeFocusable
            <&> M.tValue %~ Widget.weakerEvents
                (addItemEventMap <> addItemWithSearchTermEventMap conf env myId)
            <&> Responsive.fromWithTextPos
            & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

makeAddItem ::
    _ =>
    Config -> i (Sugar.TagChoice Name o) -> Widget.Id -> GuiM env i o (Maybe (TaggedItem o))
makeAddItem conf addItem baseId =
    GuiState.isSubCursor ?? myId <&> guard
    >>= (Lens._Just . const) (GuiM.im addItem >>= makeAddItemRow conf myId)
    where
        myId = TagEdit.addItemId baseId

make :: _ => Config -> ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make conf (Ann (Const pl) (Sugar.Composite (Sugar.TaggedList addItem mTlBody) punned compositeTail)) =
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
        items <-
            mconcat
            [ makeAddItem conf addItem myId <&> (^.. traverse)
            , foldMap (TaggedList.makeBody (has . (conf ^. itemName)) keys (pure myId) (pure myId)) mTlBody
                >>= traverse (makeItemRow conf)
                <&> concat
                <&> Lens.ix 0 . tagPre . Lens._Just . M.tValue %~ M.weakerEvents prependEventMap
            , case punned of
                [] -> pure []
                _ ->
                    M.weakerEvents
                    <$> TaggedList.addNextEventMap
                        (has . (conf ^. itemName)) (keys ^. TaggedList.kAdd) (pure punAddId)
                    <*> GetVarEdit.makePunnedVars punned
                    <&> (:[]) . (TaggedItem Nothing ?? Nothing)
                ]
        case items of
            [] -> makeUnit conf pl
            _ ->
                Styled.addValFrame <*>
                ( grammar (label (conf ^. opener))
                    M./|/ (taggedListIndent <*> addPostTags conf items >>= postProcess)
                ) <&> Widget.weakerEvents (goToParentEventMap <> tailEventMap)
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
            case compositeTail of
            Sugar.OpenCompositeTail restExpr -> makeOpenComposite conf restExpr
            _ -> pure

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
    Config -> Widget.Id ->
    Sugar.TagChoice Name o ->
    GuiM env i o (TaggedItem o)
makeAddItemRow conf tagHoleId addItem =
    TagEdit.makeTagHoleEdit mkPickResult tagHoleId addItem
    & Styled.withColor (conf ^. tagColor)
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

makeItemRow ::
    _ =>
    Config -> TaggedList.Item Name i o (ExprGui.Expr Sugar.Term i o) ->
    GuiM env i o [TaggedItem o]
makeItemRow conf item =
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
        makeAddItem conf (item ^. TaggedList.iAddAfter) myId <&> (^.. traverse) <&> (row:)
    where
        itemId = item ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance
        myId = WidgetIds.fromEntityId itemId

separationBar :: Config -> TextColors -> M.AnimId -> Widget.R -> M.View
separationBar conf theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & M.tint (theme ^# (conf ^. tailColor))
    & M.scale (M.Vector2 width 10)

makeOpenComposite :: _ => Config -> ExprGui.Expr Sugar.Term i o -> Responsive o -> GuiM env i o (Responsive o)
makeOpenComposite conf rest itemsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        restExpr <- Styled.addValPadding <*> GuiM.makeSubexpression rest
        animId <- Lens.view M.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar conf (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? itemsGui ?? restExpr

closedCompositeEventMap :: _ => Config -> Sugar.ClosedCompositeActions o -> m (EventMap (o Update))
closedCompositeEventMap conf (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.compositeOpenKeys)
    (doc env [conf ^. name, Texts.open])
