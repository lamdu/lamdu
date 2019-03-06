module Lamdu.GUI.ExpressionEdit.EventMap
    ( add
    , Options(..), defaultOptions
    , ExprInfo(..), addWith
    , extractCursor
    , detachEventMap
    , addLetEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (HasWidget(..), EventContext)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Precedence (precedence)
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExprInfo name i o = ExprInfo
    { exprInfoIsHoleResult :: Bool
    , exprInfoActions :: Sugar.NodeActions name i o
    , exprInfoMinOpPrec :: MinOpPrec
    , exprInfoIsSelected :: Bool
    }

newtype Options = Options
    { addOperatorSetHoleState :: Maybe Sugar.EntityId
    }

defaultOptions :: Options
defaultOptions =
    Options
    { addOperatorSetHoleState = Nothing
    }

exprInfoFromPl ::
    Monad i =>
    Sugar.Payload name i0 o0 ExprGui.Payload -> ExprGuiM i o (ExprInfo name i0 o0)
exprInfoFromPl pl =
    do
        isSelected <- GuiState.isSubCursor ?? WidgetIds.fromExprPayload pl
        isHoleResult <- ExprGuiM.isHoleResult
        pure ExprInfo
            { exprInfoIsHoleResult = isHoleResult
            , exprInfoActions = pl ^. Sugar.plActions
            , exprInfoMinOpPrec =
                -- Expression with parentheses intercepts all operations from inside it,
                -- But if it is itself selected then we're out of the parentheses,
                -- and its parents may take some operators.
                if pl ^. Sugar.plData . ExprGui.plNeedParens && not isSelected
                then 0
                else pl ^. Sugar.plData . ExprGui.plMinOpPrec
            , exprInfoIsSelected = isSelected
            }

add ::
    (HasWidget w, Monad i, Monad o) =>
    Options -> Sugar.Payload name i o ExprGui.Payload ->
    ExprGuiM i o (Gui w o -> Gui w o)
add options pl = exprInfoFromPl pl >>= addWith options

addWith ::
    (HasWidget w, Monad i, Monad o) =>
    Options -> ExprInfo name i o -> ExprGuiM i o (Gui w o -> Gui w o)
addWith options exprInfo =
    actionsEventMap options exprInfo <&> Widget.weakerEventsWithContext

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) = WidgetIds.fromEntityId defId

extractEventMap ::
    (MonadReader env m, Config.HasConfig env, Functor o) =>
    Sugar.NodeActions name i o -> m (Gui EventMap o)
extractEventMap actions =
    Lens.view (Config.config . Config.extractKeys)
    <&>
    \k ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor k doc
    where
        doc = E.Doc ["Edit", "Extract"]

addLetEventMap ::
    (Monad i, Monad o) =>
    o Sugar.EntityId -> ExprGuiM i o (Gui EventMap o)
addLetEventMap addLet =
    do
        config <- Lens.view Config.config
        savePos <- ExprGuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor (config ^. Config.letAddItemKeys)
                (E.Doc ["Edit", "Let clause", "Add"])
            & pure

actionsEventMap ::
    (Monad i, Monad o) =>
    Options -> ExprInfo name i o ->
    ExprGuiM i o (EventContext -> Gui EventMap o)
actionsEventMap options exprInfo =
    sequence
    [ case actions ^. Sugar.detach of
      Sugar.DetachAction act | exprInfoIsSelected exprInfo -> detachEventMap act
      _ -> pure mempty
    , if exprInfoIsHoleResult exprInfo
        then pure mempty
        else
            sequence
            [ extractEventMap actions
            , Lens.view (Config.config . Config.replaceParentKeys) <&> mkReplaceParent
            ] <&> mconcat
    , foldMap replaceEventMap (actions ^. Sugar.mSetToHole)
    , foldMap addLetEventMap (actions ^. Sugar.mNewLet)
    ] <&> mconcat <&> const
    <&> mappend (transformEventMap options exprInfo)
    where
        actions = exprInfoActions exprInfo
        mkReplaceParent replaceKeys =
            actions ^. Sugar.mReplaceParent
            & foldMap
                (E.keysEventMapMovesCursor replaceKeys (E.Doc ["Edit", "Replace parent"])
                . fmap WidgetIds.fromEntityId)

-- | Create the hole search term for new apply operators,
-- given the extra search term chars from another hole.
transformSearchTerm :: ExprInfo name i o -> EventContext -> EventMap Text
transformSearchTerm exprInfo eventCtx =
    E.charGroup Nothing (E.Doc ["Edit", "Apply Operator"]) ops Text.singleton
    <> maybeTransformEventMap
    <&> (searchStrRemainder <>)
    where
        maybeTransformEventMap
            | exprInfoIsSelected exprInfo =
                E.charEventMap "Letter" (E.Doc ["Edit", "Transform"]) transform
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
    Applicative o =>
    Options -> ExprInfo name i o -> EventContext -> Gui EventMap o
transformEventMap options exprInfo eventCtx =
    case exprInfoActions exprInfo ^. Sugar.detach of
    Sugar.DetachAction detach ->
        case addOperatorSetHoleState options of
        Just holeId -> pure holeId
        Nothing -> detach
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
    f Sugar.EntityId -> m (Gui EventMap f)
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
    f Sugar.EntityId-> m (Gui EventMap f)
replaceEventMap action =
    Lens.view Config.config
    <&>
    \config ->
    action <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Set to Hole"])
