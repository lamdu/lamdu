{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( make
    , modifyEventMap
    , jumpHolesEventMap
    ) where

import           Prelude.Compat

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.CharClassification (operatorChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleEditState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
    MonadA m => HolePickers m -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (EventHandlers (T m))
make holePickers pl =
    mconcat <$> sequenceA
    [ maybe (return mempty)
        (actionsEventMap holePickers)
        (pl ^. Sugar.plActions)
    , jumpHolesEventMapIfSelected pl
    , replaceOrComeToParentEventMap pl
    ]

mkEventMap ::
    Functor f =>
    [ModKey] -> E.Doc ->
    f Sugar.EntityId -> EventHandlers f
mkEventMap keys doc =
    Widget.keysEventMapMovesCursor keys doc .
    fmap WidgetIds.fromEntityId

mkEventMapWithPickers ::
    (Functor f, MonadA m) =>
    HolePickers m ->
    [ModKey] -> E.Doc ->
    (f Sugar.EntityId -> T m Widget.Id) ->
    f Sugar.EntityId -> EventHandlers (T m)
mkEventMapWithPickers holePickers keys doc f =
    E.keyPresses keys doc .
    liftA2 mappend (ExprGuiM.holePickersAction holePickers) .
    fmap Widget.eventResultFromCursor . f

isExprSelected :: Sugar.Payload f a -> Widget.Id -> Bool
isExprSelected pl cursor =
    WidgetIds.fromExprPayload pl
    & (`Widget.subId` cursor)
    & Lens.has Lens._Just

jumpHolesEventMap ::
    MonadA m => NearestHoles -> ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap hg =
    do
        config <- ExprGuiM.readConfig <&> Config.hole
        let jumpEventMap keys dirStr lens =
                maybe mempty
                (Widget.keysEventMapMovesCursor (keys config)
                  (E.Doc ["Navigation", jumpDoc dirStr]) . pure . WidgetIds.fromEntityId) $
                hg ^. lens
        mconcat
            [ jumpEventMap Config.holeJumpToNextKeys "next" NearestHoles.next
            , jumpEventMap Config.holeJumpToPrevKeys "previous" NearestHoles.prev
            ] & return
    where
        jumpDoc dirStr = "Jump to " ++ dirStr ++ " hole"

jumpHolesEventMapIfSelected ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (EventHandlers (T m))
jumpHolesEventMapIfSelected pl =
    do
        cursor <- ExprGuiM.widgetEnv WE.readCursor
        if isExprSelected pl cursor
            then pl ^. Sugar.plData . ExprGuiT.plNearestHoles & jumpHolesEventMap
            else pure mempty

extractEventMap :: Functor m => Config -> Sugar.Actions m -> EventHandlers (T m)
extractEventMap config actions =
    actions ^. Sugar.extract
    & maybe mempty (mkEventMap (Config.extractKeys config)
      (E.Doc ["Edit", "Extract to where-item"]))

replaceOrComeToParentEventMap ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (EventHandlers (T m))
replaceOrComeToParentEventMap pl =
    do
        config <- ExprGuiM.readConfig
        let delKeys = Config.replaceKeys config ++ Config.delKeys config
        cursor <- ExprGuiM.widgetEnv WE.readCursor
        return $
            if isExprSelected pl cursor
            then maybe mempty (replaceEventMap config) $ pl ^. Sugar.plActions
            else
                Widget.keysEventMapMovesCursor delKeys
                (E.Doc ["Navigation", "Select parent"]) selectParent
    where
        selectParent =
            WidgetIds.fromExprPayload pl
            & WidgetIds.notDelegatingId
            & return

actionsEventMap ::
    MonadA m =>
    HolePickers m ->
    Sugar.Actions m ->
    ExprGuiM m (EventHandlers (T m))
actionsEventMap holePickers actions =
    do
        config <- ExprGuiM.readConfig
        return $ mconcat
            [ wrapEventMap holePickers config
            , applyOperatorEventMap holePickers
            , extractEventMap config
            ] actions

applyOperatorEventMap ::
    MonadA m => HolePickers m -> Sugar.Actions m -> EventHandlers (T m)
applyOperatorEventMap holePickers actions =
    case actions ^. Sugar.wrap of
    Sugar.WrapAction wrap -> action wrap
    Sugar.WrapperAlready holeId -> action $ return holeId
    Sugar.WrappedAlready holeId -> action $ return holeId
    Sugar.WrapNotAllowed -> mempty
    where
        doc = E.Doc ["Edit", ExprGuiM.holePickersAddDocPrefix holePickers "Apply operator"]
        action wrap =
            E.charGroup "Operator" doc operatorChars $ \c ->
                mappend
                <$> ExprGuiM.holePickersAction holePickers
                <*> do
                    (guid, entityId) <- wrap
                    cursor <- HoleEditState.setHoleStateAndJump guid (HoleState [c]) entityId
                    return $ Widget.eventResultFromCursor cursor

wrapEventMap ::
    MonadA m =>
    HolePickers m -> Config ->
    Sugar.Actions m -> EventHandlers (T m)
wrapEventMap holePickers config actions =
    case actions ^. Sugar.wrap of
    Sugar.WrapAction wrap ->
        mkEventMapWithPickers holePickers
        (Config.wrapKeys config)
        (E.Doc ["Edit", ExprGuiM.holePickersAddDocPrefix holePickers "Wrap"])
        (fmap (HoleWidgetIds.hidOpen . HoleWidgetIds.make)) (snd <$> wrap)
    Sugar.WrapperAlready _ -> mempty
    Sugar.WrappedAlready _ -> mempty
    Sugar.WrapNotAllowed -> mempty

replaceEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (T m)
replaceEventMap config actions =
    mconcat
    [ actionEventMap (Sugar.setToInnerExpr . Sugar._SetToInnerExpr)
        "Replace with inner expression" $ Config.delKeys config
    , actionEventMap (Sugar.setToHole . Sugar._SetToHole . Lens.to (fmap snd))
        "Replace expression" delKeys
    ]
    where
        actionEventMap l doc keys =
            maybe mempty (mkEventMap keys (E.Doc ["Edit", doc])) $
            actions ^? l
        delKeys = Config.replaceKeys config ++ Config.delKeys config

modifyEventMap ::
    MonadA m => HolePickers m -> Config ->
    Sugar.Actions m -> EventHandlers (T m)
modifyEventMap holePickers config =
    mconcat
    [ wrapEventMap holePickers config
    , applyOperatorEventMap holePickers
    , replaceEventMap config
    ]
