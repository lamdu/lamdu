{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( searchTermEditEventMap
    , makeLiteralTextEventMap
    , allowedSearchTerm
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Functor.Identity (Identity(..))
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu (MetaKey(..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (HasConfig)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

searchTermEditEventMap ::
    (MonadReader env m, GuiState.HasState env, HasConfig env) =>
    WidgetIds -> m (Sugar.HoleKind f e0 e1 -> EventMap GuiState.Update)
searchTermEditEventMap widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        let appendCharEventMap =
                Text.snoc searchTerm
                & E.allChars "Character"
                (E.Doc ["Edit", "Search Term", "Append character"])
                & if Text.null searchTerm then E.filter notOp else id
        let deleteCharEventMap
                | Text.null searchTerm = mempty
                | otherwise =
                      Text.init searchTerm
                      & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                      (E.Doc ["Edit", "Search Term", "Delete backwards"])
        pure $ \holeKind ->
            appendCharEventMap <> deleteCharEventMap
            & E.filter (allowedSearchTerm holeKind)
            <&> GuiState.updateWidgetState (hidOpen widgetIds)
    where
        notOp = Text.any (`notElem` Chars.operator)

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

allowedSearchTerm :: Sugar.HoleKind f e0 e1 -> Text -> Bool
allowedSearchTerm holeKind searchTerm =
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    ] ++ kindOptions
    & any (searchTerm &)
    where
        kindOptions =
            case holeKind of
            Sugar.LeafHole{} -> [isPositiveNumber, isNegativeNumber, isLiteralBytes]
            Sugar.WrapperHole{} -> [isGetField]
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
        isGetField t =
            case Text.uncons t of
            Just (c, rest) -> c == '.' && Text.all Char.isAlphaNum rest
            Nothing -> False

-- TODO: This is ugly, maybe Sugar.HoleOption should
-- have a canonical result?
toLiteralTextEventMap ::
    Monad m =>
    Sugar.LeafHoleActions (T m) (Sugar.Expression name n a) ->
    EventMap (T m GuiState.Update)
toLiteralTextEventMap actions =
    E.keysEventMapMovesCursor toLiteralTextKeys
    (E.Doc ["Edit", "Create Text Literal"]) $
    do
        (_score, mkResult) <- Sugar.LiteralText (Identity "") & actions ^. Sugar.holeOptionLiteral
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

makeLiteralTextEventMap ::
    Monad m =>
    Sugar.HoleKind (T m) (Sugar.Expression n p a) e -> WidgetIds ->
    ExprGuiM m (EventMap (T m GuiState.Update))
makeLiteralTextEventMap holeKind widgetIds =
    HoleState.readSearchTerm widgetIds <&> f
    where
        f searchTerm
            | Text.null searchTerm = holeKind ^. Sugar._LeafHole . Lens.to toLiteralTextEventMap
            | otherwise = mempty
