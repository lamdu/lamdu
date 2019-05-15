module Lamdu.GUI.ExpressionEdit.EventMap
    ( add
    , Options(..), defaultOptions
    , ExprInfo(..), addWith
    , extractCursor
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
import           Lamdu.Config (config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
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
    ExprGuiM i o
    (Sugar.Payload name i0 o0 ExprGui.Payload -> ExprInfo name i0 o0)
exprInfoFromPl =
    (,)
    <$> GuiState.isSubCursor
    <*> ExprGuiM.isHoleResult
    <&> \(isSubCursor, isHoleResult) pl ->
    let isSelected = WidgetIds.fromExprPayload pl & isSubCursor in
    ExprInfo
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
add options pl = (exprInfoFromPl ?? pl) >>= addWith options

addWith ::
    (HasWidget w, Monad i, Monad o) =>
    Options -> ExprInfo name i o -> ExprGuiM i o (Gui w o -> Gui w o)
addWith options exprInfo =
    actionsEventMap options exprInfo <&> Widget.weakerEventsWithContext

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) = WidgetIds.fromEntityId defId

extractEventMap ::
    ( MonadReader env m, Config.HasConfig env, Language.HasLanguage env
    , Functor o
    ) =>
    m (Sugar.NodeActions name i o -> Gui EventMap o)
extractEventMap =
    Lens.view id
    <&>
    \env actions ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor (env ^. config . Config.extractKeys)
    (E.toDoc (env ^. Language.texts)
        [Texts.edit, Texts.definitions . Texts.extract])

addLetEventMap ::
    (Monad i, Monad o) =>
    o Sugar.EntityId -> ExprGuiM i o (Gui EventMap o)
addLetEventMap addLet =
    do
        env <- Lens.view id
        savePos <- ExprGuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor
            (env ^. config . Config.letAddItemKeys)
                (E.toDoc (env ^. Language.texts)
                    [ Texts.edit
                    , Texts.codeUI . Texts.letClause
                    , Texts.codeUI . Texts.add
                    ])
            & pure

actionsEventMap ::
    (Monad i, Monad o) =>
    Options -> ExprInfo name i o ->
    ExprGuiM i o (EventContext -> Gui EventMap o)
actionsEventMap options exprInfo =
    ( mconcat
        [ detachEventMap ?? exprInfo ?? actions ^. Sugar.detach
        , if exprInfoIsHoleResult exprInfo
            then pure mempty
            else
                mconcat
                [ extractEventMap ?? actions
                , mkReplaceParent
                ]
        , actions ^. Sugar.mSetToHole & foldMap replaceEventMap
        , actions ^. Sugar.mNewLet & foldMap addLetEventMap
        ] <&> const -- throw away EventContext here
    ) <> (transformEventMap ?? options ?? exprInfo)
    where
        actions = exprInfoActions exprInfo
        mkReplaceParent =
            Lens.view id
            <&> \env ->
            let replaceKeys = env ^. config . Config.replaceParentKeys in
            actions ^. Sugar.mReplaceParent
            & foldMap
                (E.keysEventMapMovesCursor replaceKeys
                    (E.toDoc (env ^. Language.texts)
                        [Texts.edit, Texts.codeUI . Texts.replaceParent])
                . fmap WidgetIds.fromEntityId)

-- | Create the hole search term for new apply operators,
-- given the extra search term chars from another hole.
transformSearchTerm ::
    (MonadReader env m, Language.HasLanguage env) =>
    m (ExprInfo name i o -> EventContext -> EventMap Text)
transformSearchTerm =
    Lens.view id <&> \env exprInfo eventCtx ->
    let maybeTransformEventMap
            | exprInfoIsSelected exprInfo =
                E.charEventMap "Letter"
                (E.toDoc (env ^. Language.texts)
                    [Texts.edit, Texts.codeUI . Texts.transform]) transform
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
    in  E.charGroup Nothing
        (E.toDoc (env ^. Language.texts)
            [Texts.edit, Texts.codeUI . Texts.applyOperator]) ops
        Text.singleton
        <> maybeTransformEventMap
        <&> (searchStrRemainder <>)

transformEventMap ::
    (MonadReader env m, Applicative o, Language.HasLanguage env) =>
    m (Options -> ExprInfo name i o -> EventContext -> Gui EventMap o)
transformEventMap =
    transformSearchTerm
    <&> \transform options exprInfo eventCtx ->
    let action detach =
            transform exprInfo eventCtx
            <&> SearchMenu.enterWithSearchTerm
            <&> (detach <&>)
    in  case exprInfoActions exprInfo ^. Sugar.detach of
        Sugar.DetachAction detach ->
            case addOperatorSetHoleState options of
            Just holeId -> pure holeId
            Nothing -> detach
        Sugar.FragmentAlready holeId -> pure holeId
        Sugar.FragmentExprAlready holeId -> pure holeId
        <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
        & action

detachEventMap ::
    ( MonadReader env m, Config.HasConfig env, Language.HasLanguage env
    , Functor f
    ) =>
    m (ExprInfo name i o -> Sugar.DetachAction f -> Gui EventMap f)
detachEventMap =
    Lens.view id
    <&>
    \env exprInfo ->
    \case
    Sugar.DetachAction act
        | exprInfoIsSelected exprInfo ->
            E.keysEventMapMovesCursor (env ^. config . Config.detachKeys)
            (E.toDoc (env ^. Language.texts) [Texts.edit, Texts.codeUI . Texts.modify])
            (act <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen)
            <>
            E.keysEventMap (env ^. config . Config.parenDetachKeys)
            (E.toDoc (env ^. Language.texts) [Texts.edit, Texts.codeUI . Texts.detach])
            (void act)
    _ -> mempty


replaceEventMap ::
    ( MonadReader env m, Config.HasConfig env, Language.HasLanguage env
    , Functor f
    ) =>
    f Sugar.EntityId-> m (Gui EventMap f)
replaceEventMap action =
    Lens.view id
    <&>
    \env ->
    action <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. config & Config.delKeys)
    (E.toDoc (env ^. Language.texts) [Texts.edit, Texts.codeUI . Texts.setToHole])
