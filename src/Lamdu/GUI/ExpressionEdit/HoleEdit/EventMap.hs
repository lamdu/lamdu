{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( disallowCharsFromSearchTerm
    , makeOpenEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Identity (Identity(..))
import           Data.List (isInfixOf)
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
    (MonadReader env m, GuiState.HasWidgetState env, Applicative f) =>
    WidgetIds -> m (E.EventMap (f GuiState.Update))
adHocTextEditEventMap widgetIds =
    HoleState.readSearchTerm widgetIds <&>
    \searchTerm ->
    let
        appendCharEventMap =
            E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            (changeText . flip Text.snoc)
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                  E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                  (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
                  changeText Text.init
        changeText f =
            GuiState.updateWidgetState (hidOpen widgetIds) (f searchTerm)
            & pure
    in
    appendCharEventMap <> deleteCharEventMap

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

allowedCharsFromSearchTerm ::
    Sugar.HoleKind f a b -> Text -> Maybe Int -> Char -> Bool
allowedCharsFromSearchTerm holeKind searchTerm mPos =
    case searchTermStr of
    "" -> allowAll
    "." -> disallow Chars.bracket
    '.':x:_
        | x `elem` Chars.operator -> allowOnly Chars.operator
        | otherwise -> disallow (Chars.operator ++ Chars.bracket)
    "-" | isLeafHole -> allowOnly (Chars.operator ++ Chars.digit)
    '-':x:_
        | x `elem` Chars.digit -> allowOnly positiveNumberChars
    "#" | isLeafHole -> allowOnly (Chars.operator ++ Chars.hexDigit)
    '#':x:_
        | x `elem` Chars.hexDigit -> allowOnly Chars.hexDigit
    x:_
        | x `elem` Chars.digit -> ['-' | Just 0 == mPos] ++ positiveNumberChars & allowOnly
        | x `elem` Chars.operator -> allowOnly Chars.operator
        | x `elem` Chars.bracket -> allowOnly Chars.bracket
        | all (`notElem` nonAlpha) searchTermStr -> disallow nonAlpha
        | otherwise ->
          error "Mix of operator/non-operator chars happened in search term?"
    where
        searchTermStr = Text.unpack searchTerm
        allowAll = const True
        allowOnly = flip elem
        disallow = flip notElem
        isLeafHole = Lens.has Sugar._LeafHole holeKind
        positiveNumberChars = Chars.digit ++ ['.' | not ("." `isInfixOf` searchTermStr)]
        nonAlpha = Chars.digit ++ Chars.operator ++ Chars.bracket

disallowCharsFromSearchTerm ::
    (MonadReader env m, HasConfig env, E.HasEventMap w) =>
    m (Sugar.HoleKind f e0 e1 -> Text -> Maybe Int -> w a -> w a)
disallowCharsFromSearchTerm =
    Lens.view Config.config <&> Config.hole <&>
    \Config.Hole{holePickAndMoveToNextHoleKeys, holePickResultKeys}
     holeKind searchTerm mPos eventMap ->
    eventMap
    & E.filterChars (allowedCharsFromSearchTerm holeKind searchTerm mPos)
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

makeOpenEventMap ::
    Monad m =>
    Sugar.HoleKind (T m) (Sugar.Expression n (T m) ()) e -> WidgetIds ->
    ExprGuiM m (E.EventMap (T m GuiState.Update))
makeOpenEventMap holeKind widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        let maybeLiteralTextEventMap
                | Text.null searchTerm = holeKind ^. Sugar._LeafHole . Lens.to toLiteralTextEventMap
                | otherwise = mempty
        (disallowCharsFromSearchTerm ?? holeKind ?? searchTerm ?? Nothing)
            <*> adHocTextEditEventMap widgetIds
            <&> mappend maybeLiteralTextEventMap
