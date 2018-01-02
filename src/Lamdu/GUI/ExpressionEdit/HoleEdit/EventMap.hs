{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( searchTermEditEventMap
    , allowedSearchTerm
    ) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (HasConfig)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

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
