{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( searchTermEditEventMap
    , allowedSearchTermCommon
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

import           Lamdu.Prelude

searchTermEditEventMap ::
    (MonadReader env m, GuiState.HasState env, HasConfig env) =>
    WidgetIds -> (Text -> Bool) -> m (EventMap GuiState.Update)
searchTermEditEventMap widgetIds allowedTerms =
    HoleState.readSearchTerm widgetIds
    <&>
    \searchTerm ->
    let appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            & if Text.null searchTerm then E.filter notOp else id
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                    Text.init searchTerm
                    & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                    (E.Doc ["Edit", "Search Term", "Delete backwards"])
    in
    appendCharEventMap <> deleteCharEventMap
    & E.filter allowedTerms
    <&> GuiState.updateWidgetState (hidOpen widgetIds)
    where
        notOp = Text.any (`notElem` Chars.operator)

allowedSearchTermCommon :: Text -> Bool
allowedSearchTermCommon searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    ]
