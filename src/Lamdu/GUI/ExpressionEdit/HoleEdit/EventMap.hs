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
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu (MetaKey(..))
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

adHocTextEditEventMap ::
    Monad m => HoleInfo m -> E.EventMap (T m Widget.EventResult)
adHocTextEditEventMap holeInfo =
    appendCharEventMap <> deleteCharEventMap
    where
        appendCharEventMap =
            E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            (changeText . flip Text.snoc)
        searchTermProp = HoleInfo.hiSearchTermProperty holeInfo
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                  E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                  (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
                  changeText Text.init
        searchTerm = Property.value searchTermProp
        changeText f = mempty <$ Property.pureModify searchTermProp f

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

allowedCharsFromSearchTerm ::
    HoleInfo m -> Maybe Int -> Char -> Bool
allowedCharsFromSearchTerm holeInfo mPos =
    case Text.unpack searchTerm of
    "" -> allowAll
    "." -> disallow Chars.bracket
    '.':x:_
        | x `elem` Chars.operator -> allowOnly Chars.operator
        | otherwise -> disallow (Chars.operator ++ Chars.bracket)
    "-" | isLeafHole -> allowOnly (Chars.operator ++ Chars.digit)
    '-':x:xs
        | x `elem` Chars.digit ->
            Chars.digit ++ ['.' | not ("." `isInfixOf` xs)] & allowOnly
    x:_
        | (x `elem` Chars.digit) && Just 0 == mPos ->
            '-':Chars.digit & allowOnly
    "#" | isLeafHole -> allowOnly (Chars.operator ++ Chars.hexDigit)
    '#':x:_
        | x `elem` Chars.hexDigit -> allowOnly Chars.hexDigit
    x:xs
        | x `elem` Chars.operator -> allowOnly Chars.operator
        | x `elem` Chars.bracket -> allowOnly Chars.bracket
        | "." `isInfixOf` xs -> allowOnly Chars.digit
        | Text.all (`elem` Chars.digit) searchTerm -> allowOnly ('.':Chars.digit)
        | Text.all (`notElem` Chars.operator) searchTerm -> disallow Chars.operator
        | otherwise ->
          error "Mix of operator/non-operator chars happened in search term?"
    where
        searchTerm = HoleInfo.hiSearchTerm holeInfo
        allowAll = const True
        allowOnly = flip elem
        disallow = flip notElem
        isLeafHole = hiHole holeInfo & Lens.has (Sugar.holeKind . Sugar._LeafHole)

disallowCharsFromSearchTerm ::
    (MonadReader env m, HasConfig env, E.HasEventMap w) =>
    m (HoleInfo f -> Maybe Int -> w a -> w a)
disallowCharsFromSearchTerm =
    Lens.view Config.config <&> Config.hole <&>
    \Config.Hole{holePickAndMoveToNextHoleKeys, holePickResultKeys}
     holeInfo mPos eventMap ->
    eventMap
    & E.filterChars (allowedCharsFromSearchTerm holeInfo mPos)
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
    E.EventMap (T m Widget.EventResult)
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
        pickedResult <- result ^. Sugar.holeResultPick
        let argExpr =
                Sugar.holeResultConverted . Sugar.rBody . Sugar._BodyHole .
                Sugar.holeKind . Sugar._WrapperHole . Sugar.haExpr
        case result ^? argExpr of
            Just arg -> arg
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & (`lookup` (pickedResult ^. Sugar.prIdTranslation))
            & fromMaybe (error "PickedResult missing translation for expr")
            & WidgetIds.fromEntityId
            & WidgetIds.literalTextEditOf
            & pure

makeOpenEventMap ::
    Monad m =>
    HoleInfo m ->
    ExprGuiM m (E.EventMap (T m Widget.EventResult))
makeOpenEventMap holeInfo =
    disallowCharsFromSearchTerm ?? holeInfo ?? Nothing
    ?? adHocTextEditEventMap holeInfo
    <&> mappend maybeLiteralTextEventMap
    where
        maybeLiteralTextEventMap
            | Text.null searchTerm =
              hiHole holeInfo ^.
                Sugar.holeKind . Sugar._LeafHole . Lens.to toLiteralTextEventMap
            | otherwise = mempty
        searchTerm = HoleInfo.hiSearchTermProperty holeInfo ^. Property.pVal
