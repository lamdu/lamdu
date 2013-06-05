{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  , HoleState(..), hsSearchTerm
  , setHoleStateAndJump
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Lens.Operators
import Control.Monad (msum, guard, join, when)
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Maybe (isJust, listToMaybe, maybeToList, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (traverse, sequenceA)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleState(..), hsSearchTerm)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results (MakeWidgets(..), ResultsList(..), Result(..), HaveHiddenResults(..))
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

extraSymbol :: String
extraSymbol = "▷"

extraSymbolSizeFactor :: Fractional a => a
extraSymbolSizeFactor = 0.5

type T = Transaction

handlePickResultTargetGuid ::
  MonadA m => HoleInfo m -> Maybe Guid -> Widget.EventResult
handlePickResultTargetGuid holeInfo =
  Widget.eventResultFromCursor . WidgetIds.fromGuid .
  fromMaybe (hiGuid holeInfo)

resultPickEventMap ::
  MonadA m => HoleInfo m -> Sugar.HoleResult Sugar.Name m ->
  Widget.EventHandlers (T m)
resultPickEventMap holeInfo holeResult =
  case hiMNextHole holeInfo of
  Just nextHole
    | not (Sugar.holeResultHasHoles holeResult) ->
      mappend (simplePickRes Config.pickResultKeys) .
      E.keyPresses Config.pickAndMoveToNextHoleKeys
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) $
        (Widget.eventResultFromCursor . WidgetIds.fromGuid)
        (nextHole ^. Sugar.rGuid) <$
        HoleResults.pick holeInfo holeResult
  _ -> simplePickRes $ Config.pickResultKeys ++ Config.pickAndMoveToNextHoleKeys
  where
    simplePickRes keys =
      E.keyPresses keys (E.Doc ["Edit", "Result", "Pick"]) .
      fmap (handlePickResultTargetGuid holeInfo) $
      HoleResults.pick holeInfo holeResult

-- We can't simply return a single widget, because the extra results
-- widget is put on the side and does not bloat the size of the
-- left-side result widget.
data ResultCompositeWidget m = ResultCompositeWidget
  { rcwMainWidget :: WidgetT m
  , rcwExtraWidget :: Maybe (WidgetT m)
  }

makeResultCompositeWidget
  :: MonadA m
  => ResultsList m
  -> ExprGuiM m
     (ResultCompositeWidget m, Maybe (Sugar.HoleResult Sugar.Name m))
makeResultCompositeWidget results = do
  mainResultWidget <-
    maybeAddExtraSymbol ((not . null . rlExtra) results) (rId mainResult) =<< rMkWidget mainResult
  (mExtraResWidget, mResult) <-
    if mainResultWidget ^. Widget.wIsFocused
    then do
      mWidget <- fmap snd <$> makeExtra
      return (mWidget, Just (rHoleResult (rlMain results)))
    else do
      cursorOnExtra <-
        ExprGuiM.widgetEnv . WE.isSubCursor $ rlExtraResultsPrefixId results
      if cursorOnExtra
        then do
          mExtra <- makeExtra
          return . unzipF $ do
            (mResult, widget) <- mExtra
            result <- mResult
            Just (widget, result)
        else return (Nothing, Nothing)
  return
    ( ResultCompositeWidget
      { rcwMainWidget = mainResultWidget
      , rcwExtraWidget = mExtraResWidget
      }
    , mResult
    )
  where
    mainResult = rlMain results
    makeExtra = makeExtraResultsWidget $ rlExtra results

makeExtraResultsWidget ::
  MonadA m => [Result m] ->
  ExprGuiM m (Maybe (Maybe (Sugar.HoleResult Sugar.Name m), WidgetT m))
makeExtraResultsWidget extraResults
  | (not . null) extraResults = Just <$> do
    (mResults, widgets) <-
      unzip <$> traverse mkResWidget extraResults
    return (msum mResults, Box.vboxAlign 0 widgets)
  | otherwise = return Nothing
  where
    mkResWidget result = do
      isOnResult <- ExprGuiM.widgetEnv $ WE.isSubCursor (rId result)
      widget <- rMkWidget result
      return (if isOnResult then Just holeResultSugar else Nothing, widget)
      where
        holeResultSugar = rHoleResult result

makeHoleResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult Sugar.Name m -> ExprGuiM m (WidgetT m)
makeHoleResultWidget holeInfo resultId holeResult =
  ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
  -- TODO: No need for this if we just add a pick result event map
  -- to the whole hole
  Widget.scale Config.holeResultScaleFactor .
  Widget.strongerEvents (resultPickEventMap holeInfo holeResult) .
  Lens.view ExpressionGui.egWidget =<<
  (ExprGuiM.makeSubexpresion 0 . Sugar.removeTypes)
  (holeResult ^. Sugar.holeResultConverted)

makeNewTagResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult Sugar.Name m ->
  ExprGuiM m (WidgetT m)
makeNewTagResultWidget holeInfo resultId holeResult = do
  widget <- makeHoleResultWidget holeInfo resultId holeResult
  ExprGuiM.widgetEnv $ do
    label <-
      fmap (Widget.scale asNewLabelSizeFactor) .
      BWidgets.makeLabel " (as new tag)" $ Widget.toAnimId resultId
    return $ Box.hboxAlign 0.5 [widget, label]

maybeAddExtraSymbol :: MonadA m => Bool -> Widget.Id -> Widget f -> ExprGuiM m (Widget f)
maybeAddExtraSymbol haveExtraResults myId w
  | haveExtraResults = do
    extraSymbolLabel <-
      fmap (Widget.scale extraSymbolSizeFactor) .
      ExprGuiM.widgetEnv .
      BWidgets.makeLabel extraSymbol $ Widget.toAnimId myId
    return $ BWidgets.hboxCenteredSpaced [w, extraSymbolLabel]
  | otherwise = return w

makeNoResults :: MonadA m => AnimId -> ExprGuiM m (WidgetT m)
makeNoResults myId =
  ExprGuiM.widgetEnv .
  BWidgets.makeTextViewWidget "(No results)" $
  mappend myId ["no results"]

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

holeResultAnimMappingNoParens :: HoleInfo m -> Widget.Id -> AnimId -> AnimId
holeResultAnimMappingNoParens holeInfo resultId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) myId .
  renamePrefix myId ("old hole" : myId)
  where
    myId = Widget.toAnimId $ hiHoleId holeInfo

asNewLabelSizeFactor :: Fractional a => a
asNewLabelSizeFactor = 0.5

mkAddNewDefinitionEventMap ::
  MonadA m => Anchors.CodeProps m -> HoleInfo m -> ExprGuiM m (Widget.EventHandlers (T m))
mkAddNewDefinitionEventMap cp holeInfo = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  mDefRef <-
    ExprGuiM.liftMemoT $
    hiHoleActions holeInfo ^. Sugar.holeResult $
    Sugar.ResultSeedNewDefinition newName
  let
    f defRef =
      E.keyPresses Config.newDefinitionKeys
      (E.Doc ["Edit", "Result", "As new Definition"]) $ do
        mTargetGuid <- HoleResults.pick holeInfo defRef
        when (isJust mTargetGuid) $
          DataOps.savePreJumpPosition cp cursor
        pure Widget.EventResult
          { Widget._eCursor = WidgetIds.fromGuid <$> mTargetGuid
          , Widget._eAnimIdMapping =
            holeResultAnimMappingNoParens holeInfo searchTermId
          }
  pure $ maybe mempty f mDefRef
  where
    searchTerm = hiState holeInfo ^. Property.pVal . hsSearchTerm
    newName = concat . words $ searchTerm
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

vboxMBiasedAlign ::
  Maybe Box.Cursor -> Box.Alignment -> [Widget f] -> Widget f
vboxMBiasedAlign mChildIndex align =
  maybe Box.toWidget Box.toWidgetBiased mChildIndex .
  Box.makeAlign align Box.vertical

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
  fmap Just . ExprGuiM.widgetEnv . BWidgets.makeLabel "..." $
  Widget.toAnimId myId
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF x = (fst <$> x, snd <$> x)

blockDownEvents :: Monad f => Widget f -> Widget f
blockDownEvents =
  Widget.weakerEvents $
  E.keyPresses
  [E.ModKey E.noMods E.KeyDown]
  (E.Doc ["Navigation", "Move", "down (blocked)"]) $
  return Widget.emptyEventResult

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveHiddenResults ->
  ExprGuiM m (Maybe (Sugar.HoleResult Sugar.Name m), WidgetT m)
makeResultsWidget holeInfo shownResults hiddenResults = do
  (widgets, mResults) <-
    unzip <$> traverse makeResultCompositeWidget shownResults
  let
    (mIndex, mResult) = unzipF $ mResults ^@? Lens.itraversed <. Lens._Just
    extraWidget = msum $ rcwExtraWidget <$> widgets
  shownResultsWidget <-
    case widgets of
    [] -> makeNoResults $ Widget.toAnimId myId
    _ ->
      return . blockDownEvents .
      vboxMBiasedAlign mIndex 0 $
      rcwMainWidget <$> widgets
  hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
  return
    ( mResult
    , BWidgets.hboxCenteredSpaced $
      Box.vboxCentered (shownResultsWidget : hiddenResultsWidgets) :
      maybeToList extraWidget
    )
  where
    myId = hiHoleId holeInfo

operatorHandler ::
  Functor f => E.Doc -> (Char -> f Widget.Id) -> Widget.EventHandlers f
operatorHandler doc handler =
  (fmap . fmap) Widget.eventResultFromCursor .
  E.charGroup "Operator" doc
  Config.operatorChars . flip $ const handler

alphaNumericHandler ::
  Functor f => E.Doc -> (Char -> f Widget.Id) -> Widget.EventHandlers f
alphaNumericHandler doc handler =
  (fmap . fmap) Widget.eventResultFromCursor .
  E.charGroup "Letter/digit" doc
  Config.alphaNumericChars . flip $ const handler

opPickEventMap ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult Sugar.Name m ->
  Widget.EventHandlers (T m)
opPickEventMap holeInfo result
  | nonEmptyAll (`notElem` Config.operatorChars) searchTerm =
    operatorHandler (E.Doc ["Edit", "Result", "Apply operator"]) $ \c -> do
      dest <- result ^. Sugar.holeResultPickWrapped
      setHoleStateAndJump (HoleState [c]) dest
  | nonEmptyAll (`elem` Config.operatorChars) searchTerm =
    alphaNumericHandler (E.Doc ["Edit", "Result", "Pick and resume"]) $ \c -> do
      mTarget <- HoleResults.pick holeInfo result
      case mTarget of
        Nothing -> pure . WidgetIds.fromGuid $ hiGuid holeInfo
        Just targetGuid -> setHoleStateAndJump (HoleState [c]) targetGuid
  | otherwise = mempty
  where
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm

mkEventMap ::
  MonadA m => HoleInfo m -> Maybe (Sugar.HoleResult Sugar.Name m) ->
  ExprGuiM m (Widget.EventHandlers (T m))
mkEventMap holeInfo mResult = do
  cp <- ExprGuiM.readCodeAnchors
  mDeleteOpResult <-
    ExprGuiM.liftMemoT . fmap join . sequenceA $ do
      guard . null $ drop 1 searchTerm
      arg <- hiMArgument holeInfo
      Just . (hiHoleActions holeInfo ^. Sugar.holeResult) .
        Sugar.ResultSeedExpression $ arg ^. Sugar.rPresugaredExpression
  addNewDefinitionEventMap <- mkAddNewDefinitionEventMap cp holeInfo
  pure $ mconcat
    [ addNewDefinitionEventMap
    , maybe mempty (opPickEventMap holeInfo) mResult
    , maybe mempty
      ( E.keyPresses Config.delKeys
        (E.Doc ["Edit", "Back"])
      . fmap (handlePickResultTargetGuid holeInfo)
      . HoleResults.pick holeInfo
      ) mDeleteOpResult
    , maybe mempty
      ( E.keyPresses Config.delKeys
        (E.Doc ["Edit", "Delete"])
      . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
      ) $ do
        guard $ null searchTerm
        actions ^. Sugar.holeMDelete
    ]
  where
    actions = hiHoleActions holeInfo
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm

assignHoleEditCursor ::
  MonadA m =>
  HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
  ExprGuiM m a ->
  ExprGuiM m a
assignHoleEditCursor holeInfo shownResultsIds allResultIds searchTermId action = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ HoleResults.prefixId holeInfo
    isOnResult = any sub allResultIds
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head (shownResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

makeActiveHoleEdit :: MonadA m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  (shownResults, hasHiddenResults) <-
    HoleResults.makeAll holeInfo MakeWidgets
    { mkNewTagResultWidget = makeNewTagResultWidget holeInfo
    , mkResultWidget = makeHoleResultWidget holeInfo
    }
  let
    shownResultsIds = rId . rlMain <$> shownResults
    allResultIds = [rId . rlMain, rlExtraResultsPrefixId] <*> shownResults
  assignHoleEditCursor
    holeInfo shownResultsIds allResultIds searchTermId $ do
      (mSelectedResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResults hasHiddenResults
      let
        mResult =
          mSelectedResult <|> rHoleResult . rlMain <$> listToMaybe shownResults
        searchTermEventMap = maybe mempty (resultPickEventMap holeInfo) mResult
      searchTermWidget <-
        makeSearchTermWidget (searchTermProperty holeInfo) searchTermId
        -- TODO: Move the result picking events into pickEventMap
        -- instead of here on the searchTerm and on each result
        & Lens.mapped . ExpressionGui.egWidget %~ Widget.strongerEvents searchTermEventMap
      holeEventMap <- mkEventMap holeInfo mResult
      maybe (return ()) (ExprGuiM.addResultPicker . (^. Sugar.holeResultPickPrefix))
        mResult
      let adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
      return .
        Lens.over ExpressionGui.egWidget
        (Widget.strongerEvents holeEventMap .
         makeBackground (hiHoleId holeInfo)
         Layers.activeHoleBG Config.holeBackgroundColor) $
        ExpressionGui.addBelow 0.5
        [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
        ]
        searchTermWidget
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

make ::
  MonadA m => Sugar.Hole Sugar.Name m (Sugar.ExpressionN m) ->
  Maybe (Sugar.ExpressionN m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole mNextHole guid =
  ExpressionGui.wrapDelegated holeFDConfig
  FocusDelegator.Delegating $ \myId -> do
    inCollapsed <- ExprGuiM.isInCollapsedExpression
    mHoleNumber <-
      if isWritable && not inCollapsed
      then Just <$> ExprGuiM.nextHoleNumber
      else pure Nothing
    makeUnwrapped mHoleNumber hole mNextHole guid myId
  where
    isWritable = isJust $ hole ^. Sugar.holeMActions

makeUnwrapped ::
  MonadA m => Maybe ExprGuiM.HoleNumber ->
  Sugar.Hole Sugar.Name m (Sugar.ExpressionN m) ->
  Maybe (Sugar.ExpressionN m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped mHoleNumber hole mNextHole guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  case (hole ^. Sugar.holeMActions, Widget.subId myId cursor) of
    (Just holeActions, Just _) -> do
      stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
      makeActiveHoleEdit HoleInfo
        { hiGuid = guid
        , hiHoleId = myId
        , hiState = stateProp
        , hiHoleActions = holeActions
        , hiMNextHole = mNextHole
        , hiMArgument = hole ^. Sugar.holeMArg
        }
    _ -> makeInactive mHoleNumber hole myId

searchTermWidgetId :: Widget.Id -> Widget.Id
searchTermWidgetId = WidgetIds.searchTermId . FocusDelegator.delegatingId

setHoleStateAndJump :: MonadA m => HoleState -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  pure . searchTermWidgetId $ WidgetIds.fromGuid newHoleGuid

assocStateRef :: MonadA m => Guid -> MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef HoleInfo.emptyState "searchTerm"

-- TODO: Use this where the hiState is currently used to get the
-- search term
searchTermProperty :: HoleInfo m -> Property (T m) String
searchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap textProp =
  mconcat . concat $
  [ [ disallowChars .
      E.simpleChars "Character"
      (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace])
      (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) textProp
    ]
  ]
  where
    changeText f = Widget.emptyEventResult <$ Property.pureModify textProp f

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

disallowChars :: E.EventMap a -> E.EventMap a
disallowChars = E.filterSChars $ curry (`notElem` disallowedHoleChars)

makeSearchTermWidget ::
  MonadA m =>
  Property (T m) String -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget searchTermProp searchTermId =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5 / Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Lens.over Widget.wEventMap disallowChars) $
  BWidgets.makeWordEdit searchTermProp searchTermId

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Hole", "Enter"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Hole", "Leave"]
  }

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

keysOfNum :: Int -> Maybe [E.ModKey]
keysOfNum n
  | 0 <= n && n <= 9 =
    map alt . (E.charKey char :) . (: []) <$> E.specialCharKey char
  | otherwise = Nothing
  where
    char = Char.intToDigit n
    alt = E.ModKey E.alt

makeInactive ::
  MonadA m => Maybe ExprGuiM.HoleNumber ->
  Sugar.Hole Sugar.Name m (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactive mHoleNumber hole myId =
  Lens.mapped . ExpressionGui.egWidget %~
  makeBackground myId Layers.inactiveHole unfocusedColor $
  case hole ^. Sugar.holeMArg of
  Just arg ->
    ExprGuiM.makeSubexpresion 0 arg
  Nothing ->
    ExpressionGui.fromValueWidget <$> do
      space <-
        ExprGuiM.widgetEnv $
        BWidgets.makeFocusableTextView "  " myId
      mHoleNumView <- traverse mkHoleNumView $ mHolePair =<< mHoleNumber
      pure $ maybe id Widget.overlayView mHoleNumView space
  where
    isWritable = isJust $ hole ^. Sugar.holeMActions
    mHolePair holeNumber = do
      keys <- keysOfNum holeNumber
      let
        doc = E.Doc ["Navigation", "Jump to", "Hole " ++ show holeNumber]
        eventMap =
          Widget.keysEventMapMovesCursor keys doc $
          pure myId
      pure ("Alt-" ++ show holeNumber, eventMap)
    mkHoleNumView (shownStr, eventMap) = do
      ExprGuiM.appendToTopLevelEventMap eventMap
      fmap (^. View.scaled Config.holeNumLabelScaleFactor) .
        ExprGuiM.withFgColor Config.holeNumLabelColor .
        ExprGuiM.widgetEnv .
        BWidgets.makeTextView shownStr $
        Widget.toAnimId myId ++ ["hole number"]
    unfocusedColor
      | isWritable = Config.holeBackgroundColor
      | otherwise = Config.readOnlyHoleBackgroundColor
