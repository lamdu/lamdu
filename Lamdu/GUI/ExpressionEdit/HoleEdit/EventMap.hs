{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( blockDownEvents, disallowChars
    , makeOpenEventMaps
    ) where

import           Control.Applicative (Applicative(..), (<$))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Foldable as Foldable
import           Data.List.Utils (nonEmptyAll)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Data.Traversable (sequenceA)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars, alphaNumericChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea.ShownResult (PickedResult(..), ShownResult(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

blockDownEvents :: Monad f => Widget f -> Widget f
blockDownEvents =
    Widget.weakerEvents $
    E.keyPresses
    [ModKey mempty GLFW.Key'Down]
    (E.Doc ["Navigation", "Move", "down (blocked)"]) $
    return mempty

pasteEventMap ::
    Functor m => Config.Hole -> EditableHoleInfo m -> Widget.EventHandlers (T m)
pasteEventMap Config.Hole{..} holeInfo =
    maybe mempty
    (Widget.keysEventMapMovesCursor
     holePasteKeys (E.Doc ["Edit", "Paste"]) .
     fmap WidgetIds.fromEntityId) $ ehiActions holeInfo ^. Sugar.holePaste

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap searchTermProp =
    appendCharEventMap <> deleteCharEventMap
    where
        appendCharEventMap =
            E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            (changeText . snoc)
            & disallowChars searchTerm
            & if null searchTerm
              then E.filterChars (`notElem` operatorChars)
              else id
        deleteCharEventMap
            | null searchTerm = mempty
            | otherwise =
                  E.keyPress (ModKey mempty GLFW.Key'Backspace)
                  (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
                  changeText init
        snoc x = (++ [x])
        searchTerm = Property.value searchTermProp
        changeText f = mempty <$ Property.pureModify searchTermProp f

disallowedHoleChars :: String
disallowedHoleChars = ",`\n() "

disallowChars :: String -> E.EventMap a -> E.EventMap a
disallowChars searchTerm =
    E.filterChars (`notElem` disallowedHoleChars) .
    deleteKeys [k GLFW.Key'Space, k GLFW.Key'Enter] .
    disallowMix
    where
        k = ModKey mempty
        disallowMix
            | nonEmptyAll (`notElem` operatorChars) searchTerm =
                  E.filterChars (`notElem` operatorChars)
            | nonEmptyAll (`elem` operatorChars) searchTerm =
                  E.filterChars (`notElem` alphaNumericChars)
            | otherwise = id

deleteKeys :: [ModKey] -> E.EventMap a -> E.EventMap a
deleteKeys = E.deleteKeys . map (E.KeyEvent GLFW.KeyState'Pressed)

pickEventMap ::
    MonadA m => Config.Hole -> HoleInfo m -> ShownResult m ->
    Widget.EventHandlers (T m)
pickEventMap Config.Hole{..} holeInfo shownResult =
    -- TODO: Does this entityId business make sense?
    case hiNearestHoles holeInfo ^. NearestHoles.next of
    Just nextHoleEntityId | not holeResultHasHoles ->
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
        holeResultHasHoles =
            srHoleResult shownResult
            & Lens.has (Sugar.holeResultConverted . SugarLens.holePayloads)
        simplePickRes keys =
            Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick"]) $
            return ()

pickBefore :: MonadA m => ShownResult m -> T m Widget.EventResult -> T m Widget.EventResult
pickBefore shownResult action =
    do
        PickedResult{..} <- srPick shownResult
        actionResult <-
            action
            <&> Widget.eCursor . Lens._Wrapped' . Lens.mapped %~
                _pickedIdTranslations
        return $ _pickedEventResult <> actionResult

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: Config -> Widget.EventHandlers f -> Widget.EventHandlers f
removeUnwanted config =
    deleteKeys (delKeys ++ gridKeyEvents ++ holeNavigationKeys)
    where
        Config.Hole{..} = Config.hole config
        holeNavigationKeys = holeOpenKeys ++ holeCloseKeys
        gridKeyEvents = Foldable.toList Grid.stdKeys
        delKeys = Config.delKeys config

mkEventsOnPickedResult :: MonadA m => ShownResult m -> ExprGuiM m (Widget.EventHandlers (T m))
mkEventsOnPickedResult shownResult =
    do
        config <- ExprGuiM.readConfig
        srMkEventMap shownResult
            <&> E.emDocs . E.docStrs . Lens._last %~ (++ "(On picked result)")
            <&> Lens.mapped %~ pickBefore shownResult
            <&> removeUnwanted config

makeOpenEventMaps ::
    MonadA m =>
    EditableHoleInfo m -> Maybe (ShownResult m) ->
    ExprGuiM m
    ( Widget.EventHandlers (T m)
    , Widget.EventHandlers (T m)
    )
makeOpenEventMaps editableHoleInfo mShownResult =
    do
        holeConfig <- ExprGuiM.readConfig <&> Config.hole
        -- below ad-hoc and search term edit:
        eventMap <-
            [ pure $ pasteEventMap holeConfig editableHoleInfo
            , case mShownResult of
              Nothing -> pure mempty
              Just shownResult ->
                  mkEventsOnPickedResult shownResult
                  <&> mappend
                  (pickEventMap holeConfig holeInfo shownResult)
            ] & sequenceA <&> mconcat
        let adHocEdit =
                adHocTextEditEventMap (HoleInfo.ehiSearchTermProperty editableHoleInfo)
        pure (eventMap, adHocEdit <> eventMap)
    where
        holeInfo = ehiInfo editableHoleInfo
