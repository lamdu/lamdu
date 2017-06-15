{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( make
    , jumpHolesEventMap
    , extractCursor
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.CharClassification (operatorChars, charPrecedence)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleEditState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

data IsHoleResult = HoleResult | NotHoleResult

make ::
    (Monad m, Monad f) =>
    Sugar.Payload f ExprGuiT.Payload -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f Widget.EventResult))
make pl holePicker =
    mconcat <$> sequenceA
    [ actionsEventMap isHoleResult pl holePicker
    , jumpHolesEventMapIfSelected pl
    , replaceOrComeToParentEventMap pl
    ]
    where
        isHoleResult
            | ExprGuiT.plOfHoleResult pl = HoleResult
            | otherwise = NotHoleResult

jumpHolesEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    NearestHoles -> m (Widget.EventMap (T f Widget.EventResult))
jumpHolesEventMap hg =
    do
        config <- Lens.view Config.config <&> Config.hole
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
        jumpDoc :: Text -> Text
        jumpDoc dirStr = "Jump to " <> dirStr <> " hole"

jumpHolesEventMapIfSelected ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Monad f) =>
    Sugar.Payload dummy ExprGuiT.Payload ->
    m (Widget.EventMap (T f Widget.EventResult))
jumpHolesEventMapIfSelected pl =
    do
        isSelected <- ExprGuiM.isExprSelected pl
        if isSelected
            then pl ^. Sugar.plData . ExprGuiT.plNearestHoles & jumpHolesEventMap
            else pure mempty

extractCursor :: Sugar.ExtractToDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) =
    WidgetIds.nameEditOf (WidgetIds.fromEntityId defId)

extractEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor f) =>
    Sugar.Actions f -> m (Widget.EventMap (T f Widget.EventResult))
extractEventMap actions =
    Lens.view Config.config <&> Config.extractKeys
    <&>
    \k ->
    actions ^. Sugar.extract <&> extractCursor
    & Widget.keysEventMapMovesCursor k doc
    where
        doc = E.Doc ["Edit", "Extract"]

replaceOrComeToParentEventMap ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Monad f) =>
    Sugar.Payload f ExprGuiT.Payload ->
    m (Widget.EventMap (T f Widget.EventResult))
replaceOrComeToParentEventMap pl =
    do
        config <- Lens.view Config.config
        isSelected <- ExprGuiM.isExprSelected pl
        return $
            if isSelected
            then replaceEventMap config (pl ^. Sugar.plActions)
            else
                Widget.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Navigation", "Select parent"]) selectParent
    where
        selectParent =
            WidgetIds.fromExprPayload pl
            & WidgetIds.notDelegatingId
            & return

actionsEventMap ::
    (Monad m, Monad f) =>
    IsHoleResult -> Sugar.Payload f ExprGuiT.Payload -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f Widget.EventResult))
actionsEventMap isHoleResult pl holePicker =
    sequence
    [ case actions ^. Sugar.wrap of
      Sugar.WrapAction act -> wrapEventMap (act <&> snd)
      _ -> return mempty
    , applyOperatorEventMap pl holePicker
    , case isHoleResult of
        HoleResult -> return mempty
        NotHoleResult -> extractEventMap actions
    , do
        config <- Lens.view Config.config
        actions ^. Sugar.mReplaceParent
            & maybe mempty
            ( Widget.keysEventMapMovesCursor
                (Config.replaceParentKeys config)
                (E.Doc ["Edit", "Replace parent"])
                . fmap WidgetIds.fromEntityId
            )
            & return
    ]
    <&> mconcat
    where
        actions = pl ^. Sugar.plActions

applyOperatorEventMap ::
    (Monad m, Monad f) =>
    Sugar.Payload f ExprGuiT.Payload -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f Widget.EventResult))
applyOperatorEventMap pl holePicker =
    do
        isSelected <- ExprGuiM.isExprSelected pl
        minOpPrec <- ExprGuiM.readMinOpPrec
        let acceptableOperatorChars
                | isSelected = operatorChars
                | otherwise =
                      filter ((>= minOpPrec) . charPrecedence) operatorChars
        let action wrap =
                E.charGroup "Operator" doc acceptableOperatorChars $ \c ->
                    do
                        (uuid, entityId) <- wrap
                        cursor <- HoleEditState.setHoleStateAndJump uuid (HoleState (Text.singleton c)) entityId
                        return $ Widget.eventResultFromCursor cursor
        case pl ^. Sugar.plActions . Sugar.wrap of
            Sugar.WrapAction wrap -> action wrap
            Sugar.WrapperAlready holeId -> action $ return holeId
            Sugar.WrappedAlready holeId -> action $ return holeId
            Sugar.WrapNotAllowed -> mempty
            & return
    <&> ExprGuiM.withHolePicker holePicker
    where
        doc = E.Doc ["Edit", "Apply operator"]

wrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    T f Sugar.EntityId -> m (Widget.EventMap (T f Widget.EventResult))
wrapEventMap wrap =
    Lens.view Config.config <&> Config.wrapKeys
    <&>
    \k ->
    Widget.keysEventMapMovesCursor k
    (E.Doc ["Edit", "Wrap"])
    (wrap <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen)

replaceEventMap ::
    Functor m =>
    Config -> Sugar.Actions m -> Widget.EventMap (T m Widget.EventResult)
replaceEventMap config actions =
    case actions ^. Sugar.setToHole of
    Sugar.SetToHole action -> mk "Delete expression" delKeys (fmap snd action)
    Sugar.SetWrapperToHole action -> mk "Delete outer hole" delKeys (fmap snd action)
    Sugar.AlreadyAHole -> mempty
    where
        mk doc keys = mkEventMap keys (E.Doc ["Edit", doc])
        delKeys = Config.delKeys config
        mkEventMap keys doc =
            Widget.keysEventMapMovesCursor keys doc .
            fmap WidgetIds.fromEntityId
