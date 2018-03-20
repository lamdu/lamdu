{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.EventMap
    ( add
    , addWithEventMap
    , addWithoutTransform
    , ExprInfo(..), addWith
    , jumpHolesEventMap
    , extractCursor
    , detachEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (HasWidget(..), EventContext)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Precedence (precedence)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExprInfo name f = ExprInfo
    { exprInfoIsHoleResult :: Bool
    , exprInfoNearestHoles :: NearestHoles
    , exprInfoActions :: Sugar.NodeActions name f
    , exprInfoMinOpPrec :: MinOpPrec
    , exprInfoIsSelected :: Bool
    }

exprInfoFromPl ::
    (MonadReader env m, GuiState.HasCursor env) =>
    Sugar.Payload name f ExprGui.Payload -> m (ExprInfo name f)
exprInfoFromPl pl =
    GuiState.isSubCursor ?? WidgetIds.fromExprPayload pl
    <&>
    \isSelected ->
    ExprInfo
    { exprInfoIsHoleResult = ExprGui.isHoleResult pl
    , exprInfoNearestHoles = pl ^. Sugar.plData . ExprGui.plNearestHoles
    , exprInfoActions = pl ^. Sugar.plActions
    , exprInfoMinOpPrec = pl ^. Sugar.plData . ExprGui.plMinOpPrec
    , exprInfoIsSelected = isSelected
    }

add ::
    (MonadReader env m, Config.HasConfig env, HasWidget w, Applicative f, GuiState.HasCursor env) =>
    Sugar.Payload name f ExprGui.Payload ->
    m (w (f GuiState.Update) -> w (f GuiState.Update))
add pl = exprInfoFromPl pl >>= addWith

addWith ::
    (MonadReader env m, Config.HasConfig env, HasWidget w, Applicative f) =>
    ExprInfo name f -> m (w (f GuiState.Update) -> w (f GuiState.Update))
addWith exprInfo =
    addWithEventMap (transformEventMap exprInfo) exprInfo

addWithoutTransform ::
    ( MonadReader env m, GuiState.HasCursor env, Config.HasConfig env
    , HasWidget w, Applicative f
    ) =>
    Sugar.Payload name f ExprGui.Payload ->
    m (w (f GuiState.Update) -> w (f GuiState.Update))
addWithoutTransform pl = exprInfoFromPl pl >>= addWithEventMap mempty

addWithEventMap ::
    (MonadReader env m, Config.HasConfig env, HasWidget w, Applicative f) =>
    (EventContext -> EventMap (f GuiState.Update)) -> ExprInfo name f ->
    m (w (f GuiState.Update) -> w (f GuiState.Update))
addWithEventMap eventMap exprInfo =
    do
        actions <- actionsEventMap exprInfo
        nav <- jumpHolesEventMap (exprInfoNearestHoles exprInfo)
        (widget . Widget.eventMapMaker . Lens.mapped <>~ nav)
            . Widget.weakerEventsWithContext (eventMap <> actions)
            & pure

jumpHolesEventMap ::
    (MonadReader env m, Config.HasConfig env, Applicative f) =>
    NearestHoles -> m (EventMap (f GuiState.Update))
jumpHolesEventMap hg =
    Lens.view (Config.config . Config.completion)
    <&>
    \config ->
    let jumpEventMap keys dirStr lens =
            case hg ^. lens of
            Nothing -> mempty
            Just dest ->
                WidgetIds.fromEntityId dest & pure
                & E.keysEventMapMovesCursor (config ^. keys)
                    (E.Doc ["Navigation", "Jump to " <> dirStr <> " hole"])
    in
    jumpEventMap Config.completionJumpToNextKeys "next" NearestHoles.next
    <>
    jumpEventMap Config.completionJumpToPrevKeys "previous" NearestHoles.prev

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId & WidgetIds.letBinderId
extractCursor (Sugar.ExtractToDef defId) = WidgetIds.fromEntityId defId

extractEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor f) =>
    Sugar.NodeActions name f -> m (EventMap (f GuiState.Update))
extractEventMap actions =
    Lens.view (Config.config . Config.extractKeys)
    <&>
    \k ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor k doc
    where
        doc = E.Doc ["Edit", "Extract"]

actionsEventMap ::
    (MonadReader env m, Config.HasConfig env, Applicative f) =>
    ExprInfo name f ->
    m (EventContext -> EventMap (f GuiState.Update))
actionsEventMap exprInfo =
    sequence
    [ case exprInfoActions exprInfo ^. Sugar.detach of
      Sugar.DetachAction act | exprInfoIsSelected exprInfo -> detachEventMap act
      _ -> pure mempty
    , if exprInfoIsHoleResult exprInfo
        then pure mempty
        else
            sequence
            [ extractEventMap (exprInfoActions exprInfo)
            , Lens.view (Config.config . Config.replaceParentKeys) <&> mkReplaceParent
            ] <&> mconcat
    , maybe (pure mempty) replaceEventMap (exprInfoActions exprInfo ^. Sugar.mSetToHole)
    ] <&> mconcat <&> const
    where
        mkReplaceParent replaceKeys =
            exprInfoActions exprInfo ^. Sugar.mReplaceParent
            & foldMap
                (E.keysEventMapMovesCursor replaceKeys (E.Doc ["Edit", "Replace parent"])
                . fmap WidgetIds.fromEntityId)

-- | Create the hole search term for new apply operators,
-- given the extra search term chars from another hole.
transformSearchTerm :: ExprInfo name f -> EventContext -> EventMap Text
transformSearchTerm exprInfo eventCtx =
    E.charGroup Nothing (E.Doc ["Edit", "Apply Operator"]) ops Text.singleton
    <> maybeTransformEventMap
    <&> (searchStrRemainder <>)
    where
        maybeTransformEventMap
            | exprInfoIsSelected exprInfo =
                E.charEventMap "Character" (E.Doc ["Edit", "Transform"]) transform
            | otherwise = mempty
        transform c =
            do
                guard (c `notElem` Chars.operator)
                guard (allowedFragmentSearchTerm (Text.singleton c))
                pure (Text.singleton c)
        searchStrRemainder = eventCtx ^. Widget.ePrevTextRemainder
        ops =
            case Text.uncons searchStrRemainder of
            Nothing -> filter acceptOp Chars.operator
            Just (firstOp, _)
                | acceptOp firstOp -> Chars.operator
                | otherwise -> mempty
        acceptOp = (>= exprInfoMinOpPrec exprInfo) . precedence

transformEventMap ::
    Applicative f =>
    ExprInfo name f -> EventContext -> EventMap (f GuiState.Update)
transformEventMap exprInfo eventCtx =
    case exprInfoActions exprInfo ^. Sugar.detach of
    Sugar.DetachAction detach -> detach
    Sugar.FragmentAlready holeId -> pure holeId
    Sugar.FragmentExprAlready holeId -> pure holeId
    <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
    & action
    where
        action detach =
            transformSearchTerm exprInfo eventCtx
            <&> SearchMenu.enterWithSearchTerm
            <&> (detach <&>)

detachEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor f) =>
    f Sugar.EntityId -> m (EventMap (f GuiState.Update))
detachEventMap detach =
    Lens.view Config.config
    <&>
    \config ->
    E.keysEventMapMovesCursor (config ^. Config.detachKeys)
    (E.Doc ["Edit", "Modify"])
    (detach <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen)
    <>
    E.keysEventMap (config ^. Config.parenDetachKeys)
    (E.Doc ["Edit", "Detach"])
    (void detach)

replaceEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor f) =>
    f Sugar.EntityId-> m (EventMap (f GuiState.Update))
replaceEventMap action =
    Lens.view Config.config
    <&>
    \config ->
    action <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Set to Hole"])
