{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( make
    , ExprInfo(..), makeWith
    , jumpHolesEventMap
    , extractCursor
    , wrapEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleEditState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Precedence (precedence)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

data ExprInfo m = ExprInfo
    { exprInfoIsHoleResult :: Bool
    , exprInfoEntityId :: Sugar.EntityId
    , exprInfoNearestHoles :: NearestHoles
    , exprInfoActions :: Sugar.Actions (T m)
    , exprInfoMinOpPrec :: MinOpPrec
    }

exprInfoFromPl :: Sugar.Payload (T f) ExprGuiT.Payload -> ExprInfo f
exprInfoFromPl pl =
    ExprInfo
    { exprInfoIsHoleResult = ExprGuiT.isHoleResult pl
    , exprInfoEntityId = pl ^. Sugar.plEntityId
    , exprInfoNearestHoles = pl ^. Sugar.plData . ExprGuiT.plNearestHoles
    , exprInfoActions = pl ^. Sugar.plActions
    , exprInfoMinOpPrec = pl ^. Sugar.plData . ExprGuiT.plMinOpPrec
    }

make ::
    (Monad m, Monad f) =>
    Sugar.Payload (T f) ExprGuiT.Payload -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f GuiState.Update))
make = makeWith . exprInfoFromPl

makeWith ::
    (Monad m, Monad f) =>
    ExprInfo f -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f GuiState.Update))
makeWith exprInfo holePicker =
    mconcat <$> sequenceA
    [ actionsEventMap exprInfo holePicker
    , jumpHolesEventMapIfSelected exprInfo
    , maybeReplaceEventMap exprInfo
    ]

jumpHolesEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    NearestHoles -> m (Widget.EventMap (T f GuiState.Update))
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

exprInfoIsSelected ::
    (Widget.HasCursor env, MonadReader env m) => ExprInfo f -> m Bool
exprInfoIsSelected exprInfo =
    Widget.isSubCursor ?? WidgetIds.fromEntityId (exprInfoEntityId exprInfo)

jumpHolesEventMapIfSelected ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Monad f) =>
    ExprInfo dummy ->
    m (Widget.EventMap (T f GuiState.Update))
jumpHolesEventMapIfSelected exprInfo =
    do
        isSelected <- exprInfoIsSelected exprInfo
        if isSelected
            then exprInfoNearestHoles exprInfo & jumpHolesEventMap
            else pure mempty

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) =
    WidgetIds.fromEntityId letId & WidgetIds.letBinderId
extractCursor (Sugar.ExtractToDef defId) =
    WidgetIds.nameEditOf (WidgetIds.fromEntityId defId)

extractEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor f) =>
    Sugar.Actions (T f) -> m (Widget.EventMap (T f GuiState.Update))
extractEventMap actions =
    Lens.view Config.config <&> Config.extractKeys
    <&>
    \k ->
    actions ^. Sugar.extract <&> extractCursor
    & Widget.keysEventMapMovesCursor k doc
    where
        doc = E.Doc ["Edit", "Extract"]

maybeReplaceEventMap ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Monad f) =>
    ExprInfo f ->
    m (Widget.EventMap (T f GuiState.Update))
maybeReplaceEventMap exprInfo =
    do
        isSelected <- exprInfoIsSelected exprInfo
        if isSelected
            then replaceEventMap (exprInfoActions exprInfo)
            else return mempty

actionsEventMap ::
    (Monad m, Monad f) =>
    ExprInfo f -> ExprGuiM.HolePicker f ->
    ExprGuiM m (Widget.EventMap (T f GuiState.Update))
actionsEventMap exprInfo holePicker =
    sequence
    [ case exprInfoActions exprInfo ^. Sugar.wrap of
      Sugar.WrapAction act -> wrapEventMap (act <&> snd)
      _ -> return mempty
    , applyOperatorEventMap exprInfo holePicker & pure
    , if exprInfoIsHoleResult exprInfo
        then return mempty
        else
            sequence
            [ extractEventMap (exprInfoActions exprInfo)
            , do
                replaceKeys <- Lens.view Config.config <&> Config.replaceParentKeys
                exprInfoActions exprInfo ^. Sugar.mReplaceParent
                    <&> void
                    & maybe mempty
                        (Widget.keysEventMap replaceKeys (E.Doc ["Edit", "Replace parent"]))
                    & return
            ] <&> mconcat
    ]
    <&> mconcat

applyOperatorEventMap ::
    Monad f =>
    ExprInfo f -> ExprGuiM.HolePicker f ->
    Widget.EventMap (T f GuiState.Update)
applyOperatorEventMap exprInfo holePicker =
    case exprInfoActions exprInfo ^. Sugar.wrap of
    Sugar.WrapAction wrap -> action wrap
    Sugar.WrapperAlready holeId -> action $ return holeId
    Sugar.WrappedAlready holeId -> action $ return holeId
    & ExprGuiM.withHolePicker holePicker
    where
        acceptableOperatorChars = filter ((>= exprInfoMinOpPrec exprInfo) . precedence) Chars.operator
        action wrap =
            E.charGroup "Operator" doc acceptableOperatorChars $
            \c ->
            do
                (uuid, entityId) <- wrap
                HoleEditState.setHoleStateAndJump uuid (Text.singleton c) entityId
            <&> GuiState.updateCursor
        doc = E.Doc ["Edit", "Apply operator"]

wrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    T f Sugar.EntityId -> m (Widget.EventMap (T f GuiState.Update))
wrapEventMap wrap =
    Lens.view Config.config
    <&>
    \config ->
    Widget.keysEventMapMovesCursor (Config.wrapKeys config)
    (E.Doc ["Edit", "Modify"])
    (wrap <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen)
    <>
    Widget.keysEventMap (Config.parenWrapKeys config)
    (E.Doc ["Edit", "Wrap with hole"])
    (void wrap)

replaceEventMap ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Monad f) =>
    Sugar.Actions (T f) -> m (Widget.EventMap (T f GuiState.Update))
replaceEventMap actions =
    do
        config <- Lens.view Config.config
        let mk action =
                action <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete expression"])
        case actions ^. Sugar.delete of
            Sugar.SetToHole action -> mk (action <&> snd)
            Sugar.Delete action -> mk action
            Sugar.CannotDelete -> mempty
            & return
