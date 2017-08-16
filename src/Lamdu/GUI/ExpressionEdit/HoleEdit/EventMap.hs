{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( blockDownEvents, blockUpEvents, disallowCharsFromSearchTerm
    , makeOpenEventMaps
    ) where

import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
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
import qualified GUI.Momentu.Widgets.Grid as Grid
import           Lamdu.CharClassification (operatorChars, bracketChars, digitChars, hexDigitChars, charPrecedence)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ShownResult (PickedResult(..), ShownResult(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
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
                (changeText . snoc)
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
        snoc x = (<> Text.singleton x)
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
disallowCharsFromSearchTerm Config.Hole{..} holeInfo mPos =
    E.filterChars (`notElem` disallowedHoleChars) .
    deleteKeys
    (holePickAndMoveToNextHoleKeys ++ holePickResultKeys <&> MetaKey.toModKey) .
    E.filterChars (allowedCharsFromSearchTerm holeInfo mPos)

deleteKeys :: [ModKey] -> E.EventMap a -> E.EventMap a
deleteKeys = E.deleteKeys . map (E.KeyEvent MetaKey.KeyState'Pressed)

pickEventMap ::
    Monad m =>
    Config.Hole -> HoleInfo m -> ShownResult m ->
    Widget.EventMap (T m Widget.EventResult)
pickEventMap Config.Hole{..} holeInfo shownResult =
    -- TODO: Does this entityId business make sense?
    case hiNearestHoles holeInfo ^. NearestHoles.next of
    Just nextHoleEntityId | not (srHasHoles shownResult) ->
        simplePickRes holePickResultKeys <>
        pickAndMoveToNextHole nextHoleEntityId
    _ ->
        simplePickRes $
        holePickResultKeys ++
        holePickAndMoveToNextHoleKeys
    <&> pickBefore shownResult
    where
        pickAndMoveToNextHole nextHoleEntityId =
            Widget.keysEventMapMovesCursor holePickAndMoveToNextHoleKeys
            (E.Doc ["Edit", "Result", "Pick and move to next hole"]) .
            return $ WidgetIds.fromEntityId nextHoleEntityId
        simplePickRes keys =
            Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick"]) $
            return ()

pickBefore :: Monad m => ShownResult m -> T m Widget.EventResult -> T m Widget.EventResult
pickBefore shownResult action =
    do
        PickedResult{..} <- srPick shownResult
        actionResult <-
            action
            <&> Widget.eCursor . Lens._Wrapped' . Lens.mapped %~
                _pickedIdTranslations
        return $ _pickedEventResult <> actionResult

-- | Remove unwanted event handlers from a hole result
removeUnwanted ::
    Monad m => ExprGuiM m (Widget.EventMap a -> Widget.EventMap a)
removeUnwanted =
    do
        config <- Lens.view Config.config
        minOpPrec <- ExprGuiM.readMinOpPrec
        let unwantedKeys =
                concat
                [ Config.delKeys config
                , Foldable.toList Grid.stdKeys
                , Config.letAddItemKeys config
                ]
                <&> MetaKey.toModKey
        let disallowedOperator '.' = False
            disallowedOperator char
                | char `notElem` operatorChars = False
                | otherwise = charPrecedence char < minOpPrec
        return (E.filterChars (not . disallowedOperator) . deleteKeys unwantedKeys)

mkEventsOnPickedResult ::
    Monad m =>
    ShownResult m -> ExprGuiM m (Widget.EventMap (T m Widget.EventResult))
mkEventsOnPickedResult shownResult =
    removeUnwanted
    <*> srMkEventMap shownResult
    <&> E.emDocs . E.docStrs . Lens._last %~ (<> " (On picked result)")
    <&> Lens.mapped %~ pickBefore shownResult

-- To make HoleEdit
emptyPickEventMap ::
    Monad f => Config.Hole -> Widget.EventMap (f Widget.EventResult)
emptyPickEventMap Config.Hole{..} =
    return ()
    & Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick (N/A)"])
    where
        keys = holePickResultKeys ++ holePickAndMoveToNextHoleKeys

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

makeOpenEventMaps ::
    Monad m =>
    HoleInfo m -> Maybe (ShownResult m) ->
    ExprGuiM m
    ( Widget.EventMap (T m Widget.EventResult)
    , Widget.EventMap (T m Widget.EventResult)
    )
makeOpenEventMaps holeInfo mShownResult =
    do
        holeConfig <- Lens.view Config.config <&> Config.hole
        -- below ad-hoc and search term edit:
        eventMap <-
            case mShownResult of
            Nothing -> pure (emptyPickEventMap holeConfig)
            Just shownResult ->
                mkEventsOnPickedResult shownResult
                <&> mappend (pickEventMap holeConfig holeInfo shownResult)
            <&> mappend maybeLiteralTextEventMap
        adHocEdit <- adHocTextEditEventMap holeInfo
        pure (eventMap, adHocEdit <> eventMap)
    where
        isWrapperHole = hiHole holeInfo & Lens.has (Sugar.holeMArg . Lens._Just)
        maybeLiteralTextEventMap
            | Text.null searchTerm && not isWrapperHole =
              toLiteralTextEventMap holeInfo
            | otherwise = mempty
        searchTerm = HoleInfo.hiSearchTermProperty holeInfo ^. Property.pVal
