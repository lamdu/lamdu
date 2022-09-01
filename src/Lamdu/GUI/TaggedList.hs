{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.TaggedList
    ( Item(..), iTag, iValue, iEventMap, iAddAfter
    , Keys(..), kAdd, kOrderBefore, kOrderAfter
    , make, makeBody, itemId, delEventMap, addNextEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended (withPrevNext)
import           GUI.Momentu (EventMap, ModKey, Update)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Prelude

data Item name i o a = Item
    { _iTag :: Sugar.TagRef name i o
    , _iValue :: a
    , _iEventMap :: EventMap (o Update)
    , _iAddAfter :: i (Sugar.TagChoice name o)
    }
Lens.makeLenses ''Item

data Keys a = Keys
    { _kAdd :: a
    , _kOrderBefore :: a
    , _kOrderAfter :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Keys

make ::
    _ =>
    Lens.ALens' env Text ->
    Keys [ModKey] ->
    o Widget.Id -> o Widget.Id ->
    Sugar.TaggedList name i o a ->
    m (EventMap (o Update), [Item name i o a])
make cat keys prevId nextId tl =
    (,)
    <$> addNextEventMap cat (keys ^. kAdd) prevId
    <*> foldMap (makeBody cat keys prevId nextId) (tl ^. Sugar.tlItems)

makeBody ::
    _ =>
    Lens.ALens' env Text ->
    Keys [ModKey] ->
    o Widget.Id -> o Widget.Id ->
    Sugar.TaggedListBody name i o a ->
    m [Item name i o a]
makeBody cat keys prevId nextId items =
    do
        env <- Lens.view id
        let addOrderAfter Nothing = id
            addOrderAfter (Just orderAfter) =
                iEventMap <>~
                E.keysEventMap (keys ^. kOrderAfter)
                (E.toDoc env [has . MomentuTexts.edit, cat, has . Texts.moveAfter])
                orderAfter
        let addDel (p, n, item) =
                item
                & iEventMap <>~ delEventMap cat (void (item ^. iValue . _1)) p n env
                & iValue %~ (^. _2)
        (:) <$> makeItem cat (keys ^. kAdd) (items ^. Sugar.tlHead)
            <*> traverse (makeSwappableItem cat keys) (items ^. Sugar.tlTail)
            <&> zipWith addOrderAfter orderAfters
            <&> withPrevNext prevId nextId (pure . itemId . (^. iTag))
            <&> Lens.mapped %~ addDel
    where
        orderAfters =
            (items ^.. Sugar.tlTail . traverse . Sugar.tsiSwapWithPrevious <&> Just) <>
            [Nothing]

delEventMap ::
    _ => Lens.ALens' env Text -> o () -> o Widget.Id -> o Widget.Id -> m (EventMap (o Update))
delEventMap cat fpDel prevId nextId =
    Lens.view id <&>
    \env ->
    let dir keys delText dstPosId =
            E.keyPresses (env ^. has . keys)
            (E.toDoc env [has . MomentuTexts.edit, cat, has . delText])
            (fpDel *> dstPosId <&> GuiState.updateCursor)
    in
    -- TODO: Imports SearchMenu just for deleteBackwards text?
    dir Config.delBackwardKeys SearchMenu.textDeleteBackwards prevId <>
    dir Config.delForwardKeys MomentuTexts.delete nextId

addNextEventMap :: _ => Lens.ALens' env Text -> [ModKey] -> o Widget.Id -> m (EventMap (o Update))
addNextEventMap cat addKeys myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor addKeys
    (E.toDoc env [has . MomentuTexts.edit, cat, has . Texts.add])
    (myId <&> TagEdit.addItemId)

makeItem ::
    _ =>
    Lens.ALens' env Text -> [ModKey] ->
    Sugar.TaggedItem name i o a -> m (Item name i o (o (), a))
makeItem cat addKeys item =
    addNextEventMap cat addKeys ((pure . itemId) (item ^. Sugar.tiTag)) <&>
    \x ->
    Item
    { _iTag = item ^. Sugar.tiTag
    , _iValue = (item ^. Sugar.tiDelete, item ^. Sugar.tiValue)
    , _iAddAfter = item ^. Sugar.tiAddAfter
    , _iEventMap = x
    }

makeSwappableItem ::
    _ =>
    Lens.ALens' env Text -> Keys [ModKey] ->
    Sugar.TaggedSwappableItem name i o a -> m (Item name i o (o (), a))
makeSwappableItem cat keys item =
    do
        env <- Lens.view id
        let eventMap =
                E.keysEventMap (keys ^. kOrderBefore)
                (E.toDoc env
                [has . MomentuTexts.edit, has . Texts.moveBefore])
                (item ^. Sugar.tsiSwapWithPrevious)
        makeItem cat (keys ^. kAdd) (item ^. Sugar.tsiItem)
            <&> iEventMap <>~ eventMap

itemId :: Sugar.TagRef name i o -> Widget.Id
itemId item = item ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
