{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( disallowCharsFromSearchTerm
    , makeSearchTermEditEventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Functor.Identity (Identity(..))
import           Data.List.Class (List)
import qualified Data.List.Class as List
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu (MetaKey(..))
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

adHocTextEditEventMap ::
    (MonadReader env m, GuiState.HasState env, HasConfig env) =>
    WidgetIds -> m (Sugar.HoleKind f e0 e1 -> E.EventMap GuiState.Update)
adHocTextEditEventMap widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        let appendCharEventMap =
                Text.snoc searchTerm
                & E.allChars "Character"
                (E.Doc ["Edit", "Search Term", "Append character"])
        let deleteCharEventMap
                | Text.null searchTerm = mempty
                | otherwise =
                      Text.init searchTerm
                      & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                      (E.Doc ["Edit", "Search Term", "Delete backwards"])
        disallow <- disallowCharsFromSearchTerm
        pure $ \holeKind ->
            appendCharEventMap <> deleteCharEventMap
            & disallow holeKind id
            <&> GuiState.updateWidgetState (hidOpen widgetIds)

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

allowedSearchTerm :: Sugar.HoleKind f e0 e1 -> Text -> Bool
allowedSearchTerm holeKind searchTerm =
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    ] ++ ( if Lens.has Sugar._LeafHole holeKind
            then [isPositiveNumber, isNegativeNumber, isLiteralBytes]
            else []
         )
    & any (searchTerm &)
    where
        isLiteralBytes :: Text -> Bool
        isLiteralBytes = prefixed '#' (Text.all Char.isHexDigit)
        isNegativeNumber = prefixed '-' isPositiveNumber
        prefixed char restPred t =
            case Text.uncons t of
            Just (c, rest) -> c == char && restPred rest
            _ -> False
        isPositiveNumber t =
            case Text.splitOn "." t of
            [digits]             -> Text.all Char.isDigit digits
            [digits, moreDigits] -> Text.all Char.isDigit (digits <> moreDigits)
            _ -> False

disallowCharsFromSearchTerm ::
    (MonadReader env m, HasConfig env, E.HasEventMap w) =>
    m (Sugar.HoleKind f e0 e1 -> (a -> Text) -> w a -> w a)
disallowCharsFromSearchTerm =
    Lens.view Config.config <&> Config.hole <&>
    \Config.Hole{holePickAndMoveToNextHoleKeys, holePickResultKeys}
     holeKind getSearchTerm eventMap ->
    eventMap
    & E.filter (allowedSearchTerm holeKind . getSearchTerm)
    & E.filterChars (`notElem` Chars.disallowedInHole)
    & deleteKeys (holePickAndMoveToNextHoleKeys ++ holePickResultKeys <&> MetaKey.toModKey)

deleteKeys :: E.HasEventMap f => [ModKey] -> f a -> f a
deleteKeys keys =
    E.eventMap %~ E.deleteKeys pressedKeys
    where
        pressedKeys = keys <&> E.KeyEvent MetaKey.KeyState'Pressed

listTHead :: List t => b -> t b -> List.ItemM t b
listTHead nil l =
    l
    & List.runList
    <&> \case
        List.Nil -> nil
        List.Cons item _ -> item

-- TODO: This is ugly, maybe Sugar.HoleOption should
-- have a canonical result?
toLiteralTextEventMap ::
    Monad m =>
    Sugar.LeafHoleActions (T m) (Sugar.Expression name (T m) ()) ->
    E.EventMap (T m GuiState.Update)
toLiteralTextEventMap actions =
    Widget.keysEventMapMovesCursor toLiteralTextKeys
    (E.Doc ["Edit", "Create Text Literal"]) $
    do
        (_score, mkResult) <-
            Sugar.LiteralText (Identity "")
            & actions ^. Sugar.holeOptionLiteral
            <&> (^. Sugar.hoResults)
            >>= listTHead (error "Literal hole option has no results?!")
        result <- mkResult
        result ^. Sugar.holeResultPick
        let argExpr =
                Sugar.holeResultConverted . Sugar.rBody . Sugar._BodyHole .
                Sugar.holeKind . Sugar._WrapperHole . Sugar.haExpr
        case result ^? argExpr of
            Just arg -> arg
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
            & WidgetIds.literalTextEditOf
            & pure

makeSearchTermEditEventMap ::
    Monad m =>
    Sugar.HoleKind (T m) (Sugar.Expression n (T m) ()) e -> WidgetIds ->
    ExprGuiM m (E.EventMap (T m GuiState.Update))
makeSearchTermEditEventMap holeKind widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        let maybeLiteralTextEventMap
                | Text.null searchTerm = holeKind ^. Sugar._LeafHole . Lens.to toLiteralTextEventMap
                | otherwise = mempty
        adHocTextEditEventMap widgetIds ?? holeKind
            <&> fmap pure
            <&> mappend maybeLiteralTextEventMap
