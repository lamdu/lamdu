module Lamdu.GUI.Expr.EventMap
    ( add, makeBaseEvents
    , Options(..), defaultOptions
    , addLetEventMap
    , makeLiteralNumberEventMap
    , makeLiteralCharEventMap
    , makeLiteralEventMap
    , allowedSearchTerm, isAlphaNumericName
    , parenKeysEvent
    , closeParenEvent
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import           GUI.Momentu (EventMap, noMods, Update)
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (EventContext)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import           Lamdu.Precedence (precedence)
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExprInfo o = ExprInfo
    { exprInfoActions :: Sugar.NodeActions o
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

exprInfoFromPl :: _ => Sugar.Payload v o0 -> GuiM env i o (ExprInfo o0)
exprInfoFromPl pl =
    WidgetIds.fromExprPayload pl & GuiState.isSubCursor <&>
    \isSelected ->
    ExprInfo
    { exprInfoActions = pl ^. Sugar.plActions
    , exprInfoMinOpPrec =
        -- Expression with parentheses intercepts all operations from inside it,
        -- But if it is itself selected then we're out of the parentheses,
        -- and its parents may take some operators.
        if pl ^. Sugar.plParenInfo . Sugar.piNeedParens && not isSelected
        then 0
        else pl ^. Sugar.plParenInfo . Sugar.piMinOpPrec
    , exprInfoIsSelected = isSelected
    }

add ::
    _ =>
    Options -> Sugar.Payload v o ->
    w o -> GuiM env i o (w o)
add options pl w =
    exprInfoFromPl pl >>= actionsEventMap options <&> Widget.weakerEventsWithContext ?? w

makeBaseEvents :: _ => Sugar.Payload v o -> GuiM env i o (EventMap (o Update))
makeBaseEvents pl = exprInfoFromPl pl >>= baseEvents

extractEventMap :: _ => Sugar.NodeActions o -> m (EventMap (o Update))
extractEventMap actions =
    Lens.view id <&>
    \env ->
    actions ^. Sugar.extract <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.extractKeys)
    (E.toDoc env
        [has . MomentuTexts.edit, has . Texts.extract])

addLetEventMap ::
    _ => o Sugar.EntityId -> GuiM env i o (EventMap (o Update))
addLetEventMap addLet =
    do
        env <- Lens.view id
        savePos <- GuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor
            (env ^. has . Config.letAddItemKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.letClause
                    , has . Texts.add
                    ])
            & pure

actionsEventMap ::
    _ =>
    Options -> ExprInfo o ->
    GuiM env i o (EventContext -> EventMap (o Update))
actionsEventMap options exprInfo =
    (baseEvents exprInfo <&> const) -- throw away EventContext here
    <> transformEventMap options exprInfo

baseEvents ::
    _ =>
    ExprInfo o ->
    GuiM env i o (EventMap (o Update))
baseEvents exprInfo =
    mconcat
    [ detachEventMap exprInfo
    , extractEventMap actions
    , mkReplaceParent
    , actions ^. Sugar.delete & replaceEventMap
    , actions ^. Sugar.mApply & foldMap applyEventMap
    , makeLiteralEventMap (actions ^. Sugar.setToLiteral)
    ]
    where
        actions = exprInfoActions exprInfo
        mkReplaceParent =
            Lens.view id
            <&> \env ->
            let replaceKeys = env ^. has . Config.replaceParentKeys in
            actions ^. Sugar.mReplaceParent
            & foldMap
                (E.keysEventMapMovesCursor replaceKeys
                    (E.toDoc env
                        [has . MomentuTexts.edit, has . Texts.replaceParent])
                . fmap WidgetIds.fromEntityId)

-- | Create the hole search term for new apply operators,
-- given the extra search term chars from another hole.

applyEventMap :: _ => o Sugar.EntityId -> GuiM env i o (EventMap (o Update))
applyEventMap action =
    Lens.view id <&>
    \env ->
    action <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor
        [noMods ModKey.Key'Space]
        (E.toDoc env
            [ has . MomentuTexts.edit
            , has . Texts.apply
            ])

transformSearchTerm ::
    _ =>
    m (ExprInfo o -> EventContext -> EventMap Text)
transformSearchTerm =
    Lens.view id <&>
    \env exprInfo eventCtx ->
    let maybeTransformEventMap
            | exprInfoIsSelected exprInfo =
                E.charEventMap "Letter"
                (E.toDoc env
                    [has . MomentuTexts.edit, has . Texts.transform]) transform
            | otherwise = mempty
        transform c =
            do
                guard (c `notElem` Chars.operator)
                guard (allowedSearchTerm (Text.singleton c))
                pure (Text.singleton c)
        searchStrRemainder = eventCtx ^. Widget.ePrevTextRemainder
        acceptOp = (>= exprInfoMinOpPrec exprInfo) . precedence
        opDoc = E.toDoc env [has . MomentuTexts.edit, has . Texts.applyOperator]
        mkOpsGroup ops = E.charGroup Nothing opDoc ops Text.singleton
        afterDot = Just . Text.singleton
    in
    case Text.uncons searchStrRemainder of
    Nothing -> mkOpsGroup (filter acceptOp Chars.operator) <> maybeTransformEventMap
    Just ('.', "")
        | acceptOp '.' -> E.charEventMap "Letter or Operator" opDoc afterDot
        | otherwise -> mempty
    Just (firstOp, _)
        | acceptOp firstOp -> mkOpsGroup Chars.operator <> maybeTransformEventMap
        | otherwise -> maybeTransformEventMap
    <&> (searchStrRemainder <>)

transformEventMap ::
    _ => Options -> ExprInfo o -> m (EventContext -> EventMap (o Update))
transformEventMap options exprInfo =
    transformSearchTerm <&>
    \transform eventCtx ->
    let x = case exprInfoActions exprInfo ^. Sugar.detach of
            Sugar.DetachAction detach ->
                addOperatorSetHoleState options & maybe detachAndOpen widgetId
                where
                    detachAndOpen =
                        detach <&> WidgetIds.fromEntityId <&> WidgetIds.fragmentHoleId
            Sugar.FragmentedAlready holeId -> widgetId holeId <&> WidgetIds.fragmentHoleId
    in
    transform exprInfo eventCtx
    <&> SearchMenu.enterWithSearchTerm
    <&> (x <&>)
    where
        widgetId = pure . WidgetIds.fromEntityId

parenKeysEvent ::
    MonadReader env m => m ([Lens.ALens' env Text] -> o a -> EventMap (o a))
parenKeysEvent =
    Lens.view id <&>
    \env texts -> E.charGroup Nothing (E.toDoc env texts) "([?" . const

detachEventMap :: _ => ExprInfo o -> m (EventMap (o Update))
detachEventMap exprInfo =
    parenKeysEvent <&>
    \mkEvent ->
    case exprInfoActions exprInfo ^. Sugar.detach of
    Sugar.DetachAction act | exprInfoIsSelected exprInfo ->
        mkEvent [has . MomentuTexts.edit, has . Texts.detach] (mempty <$ act)
    _ -> mempty

replaceEventMap :: _ => Sugar.Delete f -> m (EventMap (f Update))
replaceEventMap x =
    Lens.view id
    <&>
    \env ->
    let mk action docLens =
            action <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env [has . MomentuTexts.edit, has . docLens])
    in
    case x of
    Sugar.SetToHole action -> mk action Texts.setToHole
    Sugar.Delete action -> mk action MomentuTexts.delete
    Sugar.CannotDelete -> mempty

goToLiteral :: ElemId -> Update
goToLiteral = GuiState.updateCursor . WidgetIds.literalEditOf

makeLiteralNumberEventMap ::
    _ =>
    String -> (Sugar.Literal Identity -> o Sugar.EntityId) ->
    m (EventMap (o Update))
makeLiteralNumberEventMap prefix setToLiteral =
    makeLiteralCommon (Just "Digit") Chars.digit Texts.literalNumber
    (Sugar.LiteralNum . Identity . read . (prefix <>) . (: [])) setToLiteral
    <&> Lens.mapped . Lens.mapped %~ goToLiteral

makeLiteralCharEventMap ::
    _ =>
    Text -> (Sugar.Literal Identity -> o Sugar.EntityId) -> m (EventMap (o Update))
makeLiteralCharEventMap searchTerm setToLiteral =
    case Text.unpack searchTerm of
    ['\'', c] ->
        makeLiteralCommon Nothing "'" Texts.literalChar (const (Sugar.LiteralChar (Identity c))) setToLiteral
        <&> Lens.mapped . Lens.mapped %~ GuiState.updateCursor
    _ -> mempty

makeLiteralCommon ::
    _ =>
    Maybe Text -> String ->
    Lens.ALens' (Texts.CodeUI Text) Text ->
    (Char -> Sugar.Literal Identity) ->
    (Sugar.Literal Identity -> o Sugar.EntityId) ->
    m (EventMap (o ElemId))
makeLiteralCommon mGroupDesc chars help f setToLiteral =
    Lens.view id <&> E.toDoc <&>
    \toDoc ->
    E.charGroup mGroupDesc (toDoc [has . MomentuTexts.edit, has . Lens.cloneLens help])
    chars (fmap WidgetIds.fromEntityId . setToLiteral . f)

makeLiteralEventMap :: _ => (Sugar.Literal Identity -> o Sugar.EntityId) -> m (EventMap (o Update))
makeLiteralEventMap =
    mconcat
    [ makeLiteralCommon Nothing "\"" Texts.literalText (const (Sugar.LiteralText (Identity ""))) <&> f
    , makeLiteralCommon Nothing "#" Texts.literalBytes (const (Sugar.LiteralBytes (Identity ""))) <&> f
    , makeLiteralNumberEventMap ""
    ]
    where
        f = Lens.mapped . Lens.mapped . Lens.mapped %~ goToLiteral

allowedSearchTerm :: Text -> Bool
allowedSearchTerm searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , isNameOrPrefixed
    , (`elem` ["\\", "{"])
    , isCharSearchTerm
    ]

isCharSearchTerm :: Text -> Bool
isCharSearchTerm t =
    case Text.uncons t of
    Just ('\'', x) -> Text.length x == 1
    _ -> False

isNameOrPrefixed :: Text -> Bool
isNameOrPrefixed t =
    case Text.uncons t of
    Nothing -> True
    Just (x, xs) | x `elem` prefixes -> isAlphaNumericName xs
    Just _ -> isAlphaNumericName t
    where
        prefixes :: String
        prefixes = ".'"

isAlphaNumericName :: Text -> Bool
isAlphaNumericName suffix =
    case Text.uncons suffix of
    Nothing -> True
    Just (x, xs) -> Char.isAlpha x && Text.all Char.isAlphaNum xs

closeParenEvent ::
    (MonadReader env m, Functor f) =>
    [Lens.ALens' env Text] -> f ElemId -> m (EventMap (f Update))
closeParenEvent doc action =
    E.charGroup (Just "Close Paren")
    <$> (Lens.view id <&> (`E.toDoc` doc))
    ?? ")]"
    ?? const (action <&> GuiState.updateCursor)
