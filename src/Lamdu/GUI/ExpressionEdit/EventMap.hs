{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( make
    , Options(..), defaultOptions
    , ExprInfo(..), makeWith
    , jumpHolesEventMap
    , extractCursor
    , wrapEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.PreEvent (PreEvents, withPreEvents)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleEditState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Precedence (Prec, precedence)
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

newtype Options = Options
    { addOperatorDontWrap :: Bool
    }

defaultOptions :: Options
defaultOptions =
    Options
    { addOperatorDontWrap = False
    }

exprInfoFromPl :: Sugar.Payload (T f) ExprGui.Payload -> ExprInfo f
exprInfoFromPl pl =
    ExprInfo
    { exprInfoIsHoleResult = ExprGui.isHoleResult pl
    , exprInfoEntityId = pl ^. Sugar.plEntityId
    , exprInfoNearestHoles = pl ^. Sugar.plData . ExprGui.plNearestHoles
    , exprInfoActions = pl ^. Sugar.plActions
    , exprInfoMinOpPrec = pl ^. Sugar.plData . ExprGui.plMinOpPrec
    }

make ::
    (MonadReader env m, Monad f, Config.HasConfig env, GuiState.HasCursor env) =>
    Options -> Sugar.Payload (T f) ExprGui.Payload -> PreEvents (T f ()) ->
    m (EventMap (T f GuiState.Update))
make options = makeWith options . exprInfoFromPl

makeWith ::
    (MonadReader env m, Monad f, Config.HasConfig env, GuiState.HasCursor env) =>
    Options -> ExprInfo f -> PreEvents (T f ()) ->
    m (EventMap (T f GuiState.Update))
makeWith options exprInfo preEvents =
    mconcat <$> sequenceA
    [ actionsEventMap options exprInfo preEvents
    , jumpHolesEventMapIfSelected exprInfo
    , maybeReplaceEventMap exprInfo
    ]

jumpHolesEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    NearestHoles -> m (EventMap (T f GuiState.Update))
jumpHolesEventMap hg =
    do
        config <- Lens.view Config.config <&> Config.hole
        let jumpEventMap keys dirStr lens =
                maybe mempty
                (E.keysEventMapMovesCursor (keys config)
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
    (GuiState.HasCursor env, MonadReader env m) => ExprInfo f -> m Bool
exprInfoIsSelected exprInfo =
    GuiState.isSubCursor ?? WidgetIds.fromEntityId (exprInfoEntityId exprInfo)

jumpHolesEventMapIfSelected ::
    (MonadReader env m, Config.HasConfig env, GuiState.HasCursor env, Monad f) =>
    ExprInfo dummy ->
    m (EventMap (T f GuiState.Update))
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
    Sugar.Actions (T f) -> m (EventMap (T f GuiState.Update))
extractEventMap actions =
    Lens.view Config.config <&> Config.extractKeys
    <&>
    \k ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor k doc
    where
        doc = E.Doc ["Edit", "Extract"]

maybeReplaceEventMap ::
    (MonadReader env m, Config.HasConfig env, GuiState.HasCursor env, Monad f) =>
    ExprInfo f ->
    m (EventMap (T f GuiState.Update))
maybeReplaceEventMap exprInfo =
    do
        isSelected <- exprInfoIsSelected exprInfo
        if isSelected
            then replaceEventMap (exprInfoActions exprInfo)
            else return mempty

actionsEventMap ::
    (MonadReader env m, Monad f, Config.HasConfig env) =>
    Options -> ExprInfo f -> PreEvents (T f ()) ->
    m (EventMap (T f GuiState.Update))
actionsEventMap options exprInfo preEvents =
    sequence
    [ case exprInfoActions exprInfo ^. Sugar.wrap of
      Sugar.WrapAction act -> wrapEventMap act
      _ -> return mempty
    , applyOperatorEventMap options exprInfo preEvents & pure
    , if exprInfoIsHoleResult exprInfo
        then return mempty
        else
            sequence
            [ extractEventMap (exprInfoActions exprInfo)
            , Lens.view Config.config <&> Config.replaceParentKeys <&> mkReplaceParent
            ] <&> mconcat
    ] <&> mconcat
    where
        mkReplaceParent replaceKeys =
            exprInfoActions exprInfo ^. Sugar.mReplaceParent <&> void
            & maybe mempty (E.keysEventMap replaceKeys (E.Doc ["Edit", "Replace parent"]))

-- | Create the hole search term for new apply operators,
-- given the extra search term chars from another hole.
applyOperatorSearchTerm :: Prec -> Text -> EventMap Text
applyOperatorSearchTerm minOpPrec searchStrRemainder =
    E.charGroup Nothing (E.Doc ["Edit", "Apply operator"])
    ops (Text.singleton <&> (searchStrRemainder <>))
    where
        ops =
            case Text.uncons searchStrRemainder of
            Nothing -> filter acceptOp Chars.operator
            Just (firstOp, _)
                | acceptOp firstOp -> Chars.operator
                | otherwise -> mempty
        acceptOp = (>= minOpPrec) . precedence

applyOperatorEventMap ::
    Monad f => Options -> ExprInfo f -> PreEvents (T f ()) -> EventMap (T f GuiState.Update)
applyOperatorEventMap options exprInfo preEvents =
    case exprInfoActions exprInfo ^. Sugar.wrap of
    Sugar.WrapAction wrap ->
        if addOperatorDontWrap options
        then exprInfoEntityId exprInfo & pure
        else wrap
    Sugar.WrapperAlready holeId -> return holeId
    Sugar.WrappedAlready holeId -> return holeId
    & action
    where
        (searchStrRemainder, onEvents) = withPreEvents preEvents
        action wrap =
            applyOperatorSearchTerm (exprInfoMinOpPrec exprInfo) searchStrRemainder
            <&> HoleEditState.setHoleStateAndJump
            <&> (wrap <&>)
            & onEvents

wrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    T f Sugar.EntityId -> m (EventMap (T f GuiState.Update))
wrapEventMap wrap =
    Lens.view Config.config
    <&>
    \config ->
    E.keysEventMapMovesCursor (Config.wrapKeys config)
    (E.Doc ["Edit", "Modify"])
    (wrap <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen)
    <>
    E.keysEventMap (Config.parenWrapKeys config)
    (E.Doc ["Edit", "Wrap with hole"])
    (void wrap)

replaceEventMap ::
    (MonadReader env m, Config.HasConfig env, GuiState.HasCursor env, Monad f) =>
    Sugar.Actions (T f) -> m (EventMap (T f GuiState.Update))
replaceEventMap actions =
    do
        config <- Lens.view Config.config
        let mk action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete expression"])
        case actions ^. Sugar.delete of
            Sugar.SetToHole action -> mk (action <&> snd)
            Sugar.Delete action -> mk action
            Sugar.CannotDelete -> mempty
            & return
