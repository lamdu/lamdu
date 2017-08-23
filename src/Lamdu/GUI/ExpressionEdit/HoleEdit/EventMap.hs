{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( blockDownEvents, blockUpEvents, disallowCharsFromSearchTerm
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
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.Widget as Widget
import           Lamdu.CharClassification (operatorChars, bracketChars, digitChars, hexDigitChars)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

blockDownEvents :: (Monoid a, Applicative f, E.HasEventMap w) => w (f a) -> w (f a)
blockDownEvents = blockDirection MetaKey.Key'Down "down"

blockUpEvents :: (Monoid a, Applicative f, E.HasEventMap w) => w (f a) -> w (f a)
blockUpEvents = blockDirection MetaKey.Key'Up "up"

blockDirection ::
    (Monoid a, Applicative f, E.HasEventMap w) =>
    MetaKey.Key -> E.Subtitle -> w (f a) -> w (f a)
blockDirection key keyName =
    pure mempty
    & E.keyPresses
        [ModKey mempty key]
        (E.Doc ["Navigation", "Move", keyName <> " (blocked)"])
    & E.weakerEvents

adHocTextEditEventMap ::
    Monad m => HoleInfo m -> ExprGuiM m (Widget.EventMap (T m Widget.EventResult))
adHocTextEditEventMap holeInfo =
    do
        holeConfig <- Lens.view Config.config <&> Config.hole
        let appendCharEventMap =
                E.allChars "Character"
                (E.Doc ["Edit", "Search Term", "Append character"])
                (changeText . flip Text.snoc)
                & disallowCharsFromSearchTerm holeConfig holeInfo Nothing
                & if Text.null searchTerm
                  then E.filterChars (`notElem` operatorChars)
                  else id
        appendCharEventMap <> deleteCharEventMap & pure
    where
        searchTermProp = HoleInfo.hiSearchTermProperty holeInfo
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                  E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                  (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
                  changeText Text.init
        searchTerm = Property.value searchTermProp
        changeText f = mempty <$ Property.pureModify searchTermProp f

disallowedHoleChars :: String
disallowedHoleChars = "`\"\n "

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
    '"':_ -> allowAll
    "." -> disallow bracketChars
    '.':x:_
        | x `elem` operatorChars -> allowOnly operatorChars
        | otherwise -> disallow (operatorChars ++ bracketChars)
    "-" | isLeafHole -> allowOnly (operatorChars ++ digitChars)
    '-':x:xs
        | x `elem` digitChars ->
            digitChars ++ ['.' | not ("." `isInfixOf` xs)] & allowOnly
    x:_
        | (x `elem` digitChars) && Just 0 == mPos ->
            '-':digitChars & allowOnly
    "#" | isLeafHole -> allowOnly (operatorChars ++ hexDigitChars)
    '#':x:_
        | x `elem` hexDigitChars -> allowOnly hexDigitChars
    x:xs
        | x `elem` operatorChars -> allowOnly operatorChars
        | x `elem` bracketChars -> allowOnly bracketChars
        | "." `isInfixOf` xs -> allowOnly digitChars
        | Text.all (`elem` digitChars) searchTerm -> allowOnly ('.':digitChars)
        | Text.all (`notElem` operatorChars) searchTerm -> disallow operatorChars
        | otherwise ->
          -- Mix of operator/non-operator chars happened in search term
          -- This can happen when editing a literal text, allow everything
          allowAll
    where
        searchTerm = HoleInfo.hiSearchTerm holeInfo
        allowAll = const True
        allowOnly = flip elem
        disallow = flip notElem
        isLeafHole = hiHole holeInfo & Lens.has (Sugar.holeMArg . Lens._Nothing)

disallowCharsFromSearchTerm ::
    Config.Hole -> HoleInfo m -> Maybe Int -> E.EventMap a -> E.EventMap a
disallowCharsFromSearchTerm hole holeInfo mPos =
    E.filterChars (`notElem` disallowedHoleChars) .
    deleteKeys
    (holePickAndMoveToNextHoleKeys ++ holePickResultKeys <&> MetaKey.toModKey) .
    E.filterChars (allowedCharsFromSearchTerm holeInfo mPos)
    where
        Config.Hole{holePickAndMoveToNextHoleKeys, holePickResultKeys} = hole

deleteKeys :: [ModKey] -> E.EventMap a -> E.EventMap a
deleteKeys = E.deleteKeys . map (E.KeyEvent MetaKey.KeyState'Pressed)

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
    Monad m => HoleInfo m -> Widget.EventMap (T m Widget.EventResult)
toLiteralTextEventMap holeInfo =
    Widget.keysEventMapMovesCursor toLiteralTextKeys
    (E.Doc ["Edit", "Create Text Literal"]) $
    do
        (_score, mkResult) <-
            Sugar.LiteralText (Identity "")
            & hiHole holeInfo ^. Sugar.holeActions . Sugar.holeOptionLiteral
            <&> (^. Sugar.hoResults)
            >>= listTHead (error "Literal hole option has no results?!")
        result <- mkResult
        pickedResult <- result ^. Sugar.holeResultPick
        case result ^. Sugar.holeResultConverted . Sugar.rBody of
            Sugar.BodyHole Sugar.Hole{ Sugar._holeMArg = Just x } ->
                x ^. Sugar.haExpr
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & (`lookup` (pickedResult ^. Sugar.prIdTranslation))
            & fromMaybe (error "PickedResult missing translation for expr")
            & WidgetIds.fromEntityId
            & WidgetIds.delegatingId
            & pure

makeOpenEventMap ::
    Monad m =>
    HoleInfo m ->
    ExprGuiM m (Widget.EventMap (T m Widget.EventResult))
makeOpenEventMap holeInfo =
    adHocTextEditEventMap holeInfo
    <&> mappend maybeLiteralTextEventMap
    where
        isWrapperHole = hiHole holeInfo & Lens.has (Sugar.holeMArg . Lens._Just)
        maybeLiteralTextEventMap
            | Text.null searchTerm && not isWrapperHole =
              toLiteralTextEventMap holeInfo
            | otherwise = mempty
        searchTerm = HoleInfo.hiSearchTermProperty holeInfo ^. Property.pVal
