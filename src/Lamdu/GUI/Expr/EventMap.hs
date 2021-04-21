module Lamdu.GUI.Expr.EventMap
    ( add
    , Options(..), defaultOptions
    , extractCursor
    , addLetEventMap
    , makeLiteralEventMap, makeLiteralNumberEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (EventContext)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedSearchTerm)
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import           Lamdu.Precedence (precedence)
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExprInfo name i o = ExprInfo
    { exprInfoActions :: Sugar.NodeActions name i o
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
    GuiM env i o
    ((Sugar.Payload v name i0 o0, ExprGui.GuiPayload) -> ExprInfo name i0 o0)
exprInfoFromPl =
    GuiState.isSubCursor <&>
    \isSubCursor pl ->
    let isSelected = WidgetIds.fromExprPayload (pl ^. _1) & isSubCursor in
    ExprInfo
    { exprInfoActions = pl ^. _1 . Sugar.plActions
    , exprInfoMinOpPrec =
        -- Expression with parentheses intercepts all operations from inside it,
        -- But if it is itself selected then we're out of the parentheses,
        -- and its parents may take some operators.
        if pl ^. _2 . ExprGui.plParenInfo . Sugar.piNeedParens && not isSelected
        then 0
        else pl ^. _2 . ExprGui.plParenInfo . Sugar.piMinOpPrec
    , exprInfoIsSelected = isSelected
    }

add ::
    _ =>
    Options -> (Sugar.Payload v name i o, ExprGui.GuiPayload) ->
    GuiM env i o (w o -> w o)
add options pl =
    exprInfoFromPl ?? pl >>= actionsEventMap options <&> Widget.weakerEventsWithContext

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) = WidgetIds.fromEntityId defId

extractEventMap :: _ => m (Sugar.NodeActions name i o -> EventMap (o GuiState.Update))
extractEventMap =
    Lens.view id
    <&>
    \env actions ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor (env ^. has . Config.extractKeys)
    (E.toDoc env
        [has . MomentuTexts.edit, has . Texts.extract])

addLetEventMap ::
    _ => o Sugar.EntityId -> GuiM env i o (EventMap (o GuiState.Update))
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
    Options -> ExprInfo name i o ->
    GuiM env i o (EventContext -> EventMap (o GuiState.Update))
actionsEventMap options exprInfo =
    ( mconcat
        [ detachEventMap ?? exprInfo
        , GuiM.isHoleResult
            >>=
            \case
            True -> pure mempty
            False ->
                mconcat
                [ extractEventMap ?? actions
                , mkReplaceParent
                ]
        , actions ^. Sugar.delete & replaceEventMap
        , actions ^. Sugar.mNewLet & foldMap addLetEventMap
        , actions ^. Sugar.mApply & foldMap applyEventMap
        , makeLiteralEventMap ?? actions
        ] <&> const -- throw away EventContext here
    ) <> (transformEventMap ?? options ?? exprInfo)
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

applyEventMap :: _ => o Sugar.EntityId -> GuiM env i o (EventMap (o GuiState.Update))
applyEventMap action =
    Lens.view id <&>
    \env ->
    action <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor
        [MetaKey MetaKey.noMods MetaKey.Key'Space]
        (E.toDoc env
            [ has . MomentuTexts.edit
            , has . Texts.apply
            ])

transformSearchTerm ::
    _ =>
    m (ExprInfo name i o -> EventContext -> EventMap Text)
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
        afterDot c =
            Text.singleton c <$
            guard (allowedSearchTerm ("." <> Text.singleton c))
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
    _ => m (Options -> ExprInfo name i o -> EventContext -> EventMap (o GuiState.Update))
transformEventMap =
    transformSearchTerm <&>
    \transform options exprInfo eventCtx ->
    let x = case exprInfoActions exprInfo ^. Sugar.detach of
            Sugar.DetachAction detach ->
                addOperatorSetHoleState options & maybe detachAndOpen widgetId
                where
                    detachAndOpen =
                        detach <&> WidgetIds.fromEntityId <&> WidgetIds.fragmentHoleId
            Sugar.FragmentAlready holeId -> widgetId holeId <&> WidgetIds.fragmentHoleId
            Sugar.FragmentExprAlready holeId -> widgetId holeId <&> WidgetIds.fragmentHoleId
    in
    transform exprInfo eventCtx
    <&> SearchMenu.enterWithSearchTerm
    <&> (x <&> HoleWidgetIds.makeFrom <&> HoleWidgetIds.hidOpen <&>)
    where
        widgetId = pure . WidgetIds.fromEntityId

detachEventMap :: _ => m (ExprInfo name i o -> EventMap (o GuiState.Update))
detachEventMap =
    Lens.view id
    <&>
    \env exprInfo ->
    case exprInfoActions exprInfo ^. Sugar.detach of
    Sugar.DetachAction act
        | exprInfoIsSelected exprInfo ->
            E.charGroup (Just "Open Paren")
            (E.toDoc env [has . MomentuTexts.edit, has . Texts.detach])
            parenKeys (const (mempty <$ act))
        where
            parenKeys =
                case env ^. has of
                Dir.LeftToRight -> "(["
                Dir.RightToLeft -> ")]"
    _ -> mempty

replaceEventMap :: _ => Sugar.Delete f -> m (EventMap (f GuiState.Update))
replaceEventMap x =
    Lens.view id
    <&>
    \env ->
    let mk action =
            action <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.setToHole])
    in
    case x of
    Sugar.SetToHole action -> mk action
    Sugar.Delete action -> mk action
    Sugar.CannotDelete -> mempty

goToLiteral :: Sugar.EntityId -> GuiState.Update
goToLiteral = GuiState.updateCursor . WidgetIds.literalEditOf . WidgetIds.fromEntityId

makeLiteralNumberEventMap ::
    _ =>
    String ->
    m ((Sugar.Literal Identity -> o Sugar.EntityId) -> EventMap (o GuiState.Update))
makeLiteralNumberEventMap prefix =
    Lens.view id <&> E.toDoc
    <&> \toDoc makeLiteral ->
    E.charGroup (Just "Digit")
    (toDoc [has . MomentuTexts.edit, has . Texts.literalNumber])
    Chars.digit
    (fmap goToLiteral . makeLiteral . Sugar.LiteralNum . Identity . read . (prefix <>) . (: []))

makeLiteralTextEventMap ::
    _ => m ((Sugar.Literal Identity -> o Sugar.EntityId) -> EventMap (o GuiState.Update))
makeLiteralTextEventMap =
    Lens.view id <&> E.toDoc <&>
    \toDoc makeLiteral ->
    E.charGroup Nothing
    (toDoc [has . MomentuTexts.edit, has . Texts.literalText]) "\""
    (const (makeLiteral (Sugar.LiteralText (Identity "")) <&> goToLiteral))

makeRecordEventMap :: _ => m (o Sugar.EntityId -> EventMap (o GuiState.Update))
makeRecordEventMap =
    Lens.view id <&>
    \env makeRec ->
    E.charGroup Nothing
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.record])
    ( case env ^. has of
        Dir.LeftToRight -> "{"
        Dir.RightToLeft -> "}"
    ) (const (makeRec <&> WidgetIds.fromEntityId <&> GuiState.updateCursor))

makeLiteralEventMap :: _ => m (Sugar.NodeActions name i o -> EventMap (o GuiState.Update))
makeLiteralEventMap =
    (<>)
    <$> ( (<>) <$> makeLiteralTextEventMap <*> makeLiteralNumberEventMap ""
            <&> (. (^. Sugar.setToLiteral))
        )
    <*> (makeRecordEventMap <&> (. (^. Sugar.setToEmptyRecord)))
