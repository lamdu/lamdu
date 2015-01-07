{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Open.EventMap
  ( make, blockDownEvents, disallowChars
  ) where

import           Control.Applicative (Applicative(..), (<$), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Foldable as Foldable
import           Data.List.Utils (nonEmptyAll)
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars, alphaNumericChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open.ShownResult (PickedResult(..), ShownResult(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
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

closeEventMap :: MonadA m => HoleInfo m -> Widget.EventHandlers (T m)
closeEventMap holeInfo =
  Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Escape]
  (E.Doc ["Navigation", "Hole", "Close"]) . pure $
  Widget.joinId (hiId holeInfo) ["closed"]

pasteEventMap ::
  Functor m => Config -> HoleInfo m -> Widget.EventHandlers (T m)
pasteEventMap config holeInfo =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromEntityId) $ hiActions holeInfo ^. Sugar.holePaste

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap searchTermProp =
  mconcat . concat $
  [ [ disallowChars (Property.value searchTermProp) .
      E.simpleChars "Character"
      (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (ModKey mempty) [GLFW.Key'Backspace])
      (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) searchTermProp
    ]
  ]
  where
    changeText f = mempty <$ Property.pureModify searchTermProp f

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

disallowChars :: String -> E.EventMap a -> E.EventMap a
disallowChars searchTerm =
  E.filterSChars (curry (`notElem` disallowedHoleChars)) .
  deleteKeys [k GLFW.Key'Space, k GLFW.Key'Enter] .
  disallowMix
  where
    k = ModKey mempty
    disallowMix
      | nonEmptyAll (`notElem` operatorChars) searchTerm =
        E.filterSChars (curry (`notElem` E.anyShiftedChars operatorChars))
      | nonEmptyAll (`elem` operatorChars) searchTerm =
        E.filterSChars (curry (`notElem` E.anyShiftedChars alphaNumericChars))
      | otherwise = id

deleteKeys :: [ModKey] -> E.EventMap a -> E.EventMap a
deleteKeys = E.deleteKeys . map (E.KeyEvent GLFW.KeyState'Pressed)

-- This relies on pickBefore being applied to it in the event map
-- buildup to do the actual picking
pickPlaceholderEventMap ::
  MonadA m => Config.Hole -> HoleInfo m -> ShownResult m ->
  Widget.EventHandlers (T m)
pickPlaceholderEventMap Config.Hole{..} holeInfo shownResult =
  -- TODO: Does this entityId business make sense?
  case hiNearestHoles holeInfo ^. NearestHoles.next of
  Just nextHoleEntityId | not holeResultHasHoles ->
    mappend
    (simplePickRes holePickResultKeys)
    (pickAndMoveToNextHole nextHoleEntityId)
  _ ->
    simplePickRes $
    holePickResultKeys ++
    holePickAndMoveToNextHoleKeys
  where
    pickAndMoveToNextHole nextHoleEntityId =
      Widget.keysEventMapMovesCursor holePickAndMoveToNextHoleKeys
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) .
      return $ WidgetIds.fromEntityId nextHoleEntityId
    holeResultHasHoles =
      Lens.has (Sugar.holeResultHoleTarget . Lens._Just) $ srHoleResult shownResult
    simplePickRes keys = Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick"]) $ return ()

setNextHoleState ::
  MonadA m => String -> PickedResult -> T m Widget.EventResult
setNextHoleState searchTerm PickedResult{..} =
  do
    case _pickedInnerHoleGuid of
      Nothing -> return ()
      Just newHoleGuid ->
        Transaction.setP (HoleState.assocStateRef newHoleGuid) (HoleState searchTerm)
    return _pickedEventResult

alphaNumericAfterOperator :: MonadA m => HoleInfo m -> ShownResult m -> Widget.EventHandlers (T m)
alphaNumericAfterOperator holeInfo shownResult
  | nonEmptyAll (`elem` operatorChars) searchTerm =
    E.charGroup "Letter/digit"
    (E.Doc ["Edit", "Result", "Pick and resume"]) alphaNumericChars $
    \c _ -> setNextHoleState [c] =<< srPick shownResult
  | otherwise = mempty
  where
    searchTerm = HoleInfo.hiSearchTerm holeInfo

pickBefore :: MonadA m => ShownResult m -> T m Widget.EventResult -> T m Widget.EventResult
pickBefore shownResult action =
  do
    PickedResult{..} <- srPick shownResult
    actionResult <-
      action
      <&> Widget.eCursor . Lens._Wrapped' . Lens.mapped %~ _pickedIdTranslations
    return $ _pickedEventResult `mappend` actionResult

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: Config -> Widget.EventHandlers f -> Widget.EventHandlers f
removeUnwanted config =
  deleteKeys (delKeys ++ gridKeyEvents)
  where
    gridKeyEvents = Foldable.toList Grid.stdKeys
    delKeys = Config.delKeys config

make ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  HoleInfo m -> Maybe (ShownResult m) ->
  ExprGuiM m
  ( Widget.EventHandlers (T m)
  , Widget.EventHandlers (T m)
  )
make pl holeInfo mShownResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  jumpHoles <- ExprEventMap.jumpHolesEventMapIfSelected [] pl
  replace <- ExprEventMap.replaceOrComeToParentEventMap True pl
  let
    applyOp = actionsEventMap $ ExprEventMap.applyOperatorEventMap []
    close = closeEventMap holeInfo
    cut = actionsEventMap $ ExprEventMap.cutEventMap config
    paste = pasteEventMap config holeInfo
    pick = shownResultEventMap $ pickPlaceholderEventMap (Config.hole config) holeInfo
    alphaAfterOp = onShownResult $ alphaNumericAfterOperator holeInfo
    adHocEdit = adHocTextEditEventMap $ HoleInfo.hiSearchTermProperty holeInfo
    strongEventMap =
      mconcat $
      jumpHoles : close : pick : alphaAfterOp :
      [ applyOp | null searchTerm ]
    weakEventMap =
      mconcat $
      [ applyOp | not (null searchTerm) ] ++
      [ cut, paste, replace
      -- includes overlapping events like "cut" of sub-expressions
      -- (since top-level expression gets its actions cut), so put
      -- at lowest precedence:
      , removeUnwanted config $ shownResultEventMap srEventMap
      ]
    -- Used with weaker events, TextEdit events above:
    searchTermEventMap = mappend strongEventMap weakEventMap
    -- Used with stronger events, Grid events underneath:
    resultsEventMap =
      mconcat [ strongEventMap, adHocEdit, weakEventMap ]
  pure (searchTermEventMap, resultsEventMap)
  where
    searchTerm = HoleInfo.hiSearchTerm holeInfo
    onShownResult f = maybe mempty f mShownResult
    shownResultEventMapH f shownResult = pickBefore shownResult <$> f shownResult
    shownResultEventMap = onShownResult . shownResultEventMapH
    actionsEventMap f =
      shownResultEventMap $ \shownResult ->
      let
        mActions =
          srHoleResult shownResult ^.
          Sugar.holeResultConverted . Sugar.rPayload . Sugar.plActions
      in case mActions of
        Nothing -> mempty
        Just actions -> f actions
