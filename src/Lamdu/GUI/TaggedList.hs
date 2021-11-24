{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.TaggedList
    ( Item(..), iTag, iValue, iEventMap, iAddAfter
    , make, itemId, delEventMap, addNextEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended (withPrevNext)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.EventMap (EventMap)
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
    , _iEventMap :: EventMap (o GuiState.Update)
    , _iAddAfter :: i (Sugar.TagChoice name o)
    }
Lens.makeLenses ''Item

make ::
    _ =>
    Lens.ALens' env Text ->
    Widget.Id -> Widget.Id ->
    Sugar.TaggedListBody name i o a ->
    m [Item name i o a]
make cat prevId nextId items =
    do
        env <- Lens.view id
        let addOrderAfter Nothing = id
            addOrderAfter (Just orderAfter) =
                iEventMap <>~
                E.keysEventMap (env ^. has . Config.paramOrderAfterKeys)
                (E.toDoc env [has . MomentuTexts.edit, cat, has . Texts.moveAfter])
                orderAfter
        let addDel (p, n, item) =
                item
                & iEventMap <>~ delEventMap cat (void (item ^. iValue . _1)) p n env
                & iValue %~ (^. _2)
        (:) <$> makeItem cat (items ^. Sugar.tlHead)
            <*> traverse (makeSwappableItem cat) (items ^. Sugar.tlTail)
            <&> zipWith addOrderAfter orderAfters
            <&> withPrevNext prevId nextId (itemId . (^. iTag))
            <&> Lens.mapped %~ addDel
    where
        orderAfters =
            (items ^.. Sugar.tlTail . traverse . Sugar.tsiSwapWithPrevious <&> Just) <>
            [Nothing]

delEventMap ::
    _ => Lens.ALens' env Text -> o () -> Widget.Id -> Widget.Id -> m (EventMap (o GuiState.Update))
delEventMap cat fpDel prevId nextId =
    Lens.view id <&>
    \env ->
    let dir keys delText dstPosId =
            E.keyPresses (env ^. has . keys)
            (E.toDoc env [has . MomentuTexts.edit, cat, has . delText])
            (GuiState.updateCursor dstPosId <$ fpDel)
    in
    -- TODO: Imports SearchMenu just for deleteBackwards text?
    dir Config.delBackwardKeys SearchMenu.textDeleteBackwards prevId <>
    dir Config.delForwardKeys MomentuTexts.delete nextId

addNextEventMap :: _ => Lens.ALens' env Text -> Widget.Id -> m _
addNextEventMap cat myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, cat, has . Texts.add])
    (pure (TagEdit.addItemId myId))

makeItem :: _ => Lens.ALens' env Text -> Sugar.TaggedItem name i o a -> m (Item name i o (o (), a))
makeItem cat item =
    addNextEventMap cat (itemId (item ^. Sugar.tiTag)) <&>
    \x ->
    Item
    { _iTag = item ^. Sugar.tiTag
    , _iValue = (item ^. Sugar.tiDelete, item ^. Sugar.tiValue)
    , _iAddAfter = item ^. Sugar.tiAddAfter
    , _iEventMap = x
    }

makeSwappableItem ::
    _ => Lens.ALens' env Text -> Sugar.TaggedSwappableItem name i o a -> m (Item name i o (o (), a))
makeSwappableItem cat item =
    do
        env <- Lens.view id
        let eventMap =
                E.keysEventMap (env ^. has . Config.paramOrderBeforeKeys)
                (E.toDoc env
                [has . MomentuTexts.edit, has . Texts.moveBefore])
                (item ^. Sugar.tsiSwapWithPrevious)
        makeItem cat (item ^. Sugar.tsiItem)
            <&> iEventMap <>~ eventMap

itemId :: Sugar.TagRef name i o -> Widget.Id
itemId item = item ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
