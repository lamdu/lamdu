{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.TaggedList
    ( Item(..), iTag, iValue, iEventMap, iAddAfter
    , make, itemId
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended (withPrevNext)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
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
    Widget.Id -> Widget.Id ->
    Sugar.TaggedListBody name i o a ->
    m [Item name i o a]
make prevId nextId items =
    do
        env <- Lens.view id
        let addOrderAfter Nothing = id
            addOrderAfter (Just orderAfter) =
                iEventMap <>~
                E.keysEventMap (env ^. has . Config.paramOrderAfterKeys)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.addNextParameter])
                orderAfter
        let addDel (p, n, item) =
                item
                & iEventMap <>~ delEventMap env (() <$ item ^. iValue . _1) p n
                & iValue %~ (^. _2)
        (:) <$> makeItem (items ^. Sugar.tlHead)
            <*> traverse makeSwappableItem (items ^. Sugar.tlTail)
            <&> zipWith addOrderAfter orderAfters
            <&> withPrevNext prevId nextId (itemId . (^. iTag))
            <&> Lens.mapped %~ addDel
    where
        orderAfters =
            (items ^.. Sugar.tlTail . traverse . Sugar.tsiSwapWithPrevious <&> Just) <>
            [Nothing]

delEventMap :: _ => env -> o () -> Widget.Id -> Widget.Id -> EventMap (o GuiState.Update)
delEventMap env fpDel prevId nextId =
    dir Config.delBackwardKeys Texts.deleteParameterBackwards prevId <>
    dir Config.delForwardKeys Texts.deleteParameter nextId
    where
        dir keys delParam dstPosId =
            GuiState.updateCursor dstPosId <$ fpDel
            & E.keyPresses (env ^. has . keys) (E.toDoc env [has . MomentuTexts.edit, has . delParam])

makeItem :: _ => Sugar.TaggedItem name i o a -> m (Item name i o (o (), a))
makeItem item =
    Lens.view id <&>
    \env ->
    Item
    { _iTag = item ^. Sugar.tiTag
    , _iValue = (item ^. Sugar.tiDelete, item ^. Sugar.tiValue)
    , _iAddAfter = item ^. Sugar.tiAddAfter
    , _iEventMap =
        E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
        (E.toDoc env [has . MomentuTexts.edit, has . Texts.addNextParameter])
        (pure (TagEdit.addParamId (itemId (item ^. Sugar.tiTag))))
    }

makeSwappableItem :: _ => Sugar.TaggedSwappableItem name i o a -> m (Item name i o (o (), a))
makeSwappableItem item =
    do
        env <- Lens.view id
        let eventMap =
                E.keysEventMap (env ^. has . Config.paramOrderBeforeKeys)
                (E.toDoc env
                [has . MomentuTexts.edit, has . Texts.moveBefore])
                (item ^. Sugar.tsiSwapWithPrevious)
        makeItem (item ^. Sugar.tsiItem)
            <&> iEventMap <>~ eventMap

itemId :: Sugar.TagRef name i o -> Widget.Id
itemId item = item ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
