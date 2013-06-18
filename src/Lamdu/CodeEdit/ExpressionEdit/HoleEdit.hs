{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
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
import Lamdu.CharClassification (operatorChars, alphaNumericChars)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleState(..), hsSearchTerm)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results (MakeWidgets(..), ResultsList(..), Result(..), HaveHiddenResults(..))
import Lamdu.Config (Config)
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
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

type T = Transaction

handlePickResultTargetGuid ::
  MonadA m => HoleInfo m -> Maybe Guid -> Widget.EventResult
handlePickResultTargetGuid holeInfo =
  Widget.eventResultFromCursor . WidgetIds.fromGuid .
  fromMaybe (hiGuid holeInfo)

resultPickEventMap ::
  MonadA m => Config -> HoleInfo m -> Sugar.HoleResult Sugar.Name m ->
  Widget.EventHandlers (T m)
resultPickEventMap config holeInfo holeResult =
  case hiMNextHoleGuid holeInfo of
  Just nextHoleGuid
    | not (Sugar.holeResultHasHoles holeResult) ->
      mappend (simplePickRes (Config.pickResultKeys config)) .
      E.keyPresses (Config.pickAndMoveToNextHoleKeys config)
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) $
        (Widget.eventResultFromCursor . WidgetIds.fromGuid)
        nextHoleGuid <$ HoleResults.pick holeInfo holeResult
  _ ->
    simplePickRes $
    Config.pickResultKeys config ++
    Config.pickAndMoveToNextHoleKeys config
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
    maybeAddExtraSymbol (Lens.has (HoleResults.rlExtra . traverse) results) (rId mainResult) =<< rMkWidget mainResult
  (mExtraResWidget, mResult) <-
    if mainResultWidget ^. Widget.wIsFocused
    then do
      mWidget <- fmap snd <$> makeExtra
      return (mWidget, Just (rHoleResult (results ^. HoleResults.rlMain)))
    else do
      cursorOnExtra <-
        ExprGuiM.widgetEnv . WE.isSubCursor $ results ^. HoleResults.rlExtraResultsPrefixId
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
    mainResult = results ^. HoleResults.rlMain
    makeExtra = makeExtraResultsWidget $ results ^. HoleResults.rlExtra

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
makeHoleResultWidget holeInfo resultId holeResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
    -- TODO: No need for this if we just add a pick result event map
    -- to the whole hole
    Widget.scale (realToFrac <$> Config.holeResultScaleFactor config) .
    Widget.strongerEvents (resultPickEventMap config holeInfo holeResult) .
    (^. ExpressionGui.egWidget) =<<
    (ExprGuiM.makeSubexpresion 0 . Sugar.removeHoleResultTypes)
    (holeResult ^. Sugar.holeResultConverted)

asNewLabelScaleFactor :: Fractional a => a
asNewLabelScaleFactor = 0.5

makeNewTagResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult Sugar.Name m ->
  ExprGuiM m (WidgetT m)
makeNewTagResultWidget holeInfo resultId holeResult = do
  widget <- makeHoleResultWidget holeInfo resultId holeResult
  ExprGuiM.widgetEnv $ do
    label <-
      fmap (Widget.scale asNewLabelScaleFactor) .
      BWidgets.makeLabel " (as new tag)" $ Widget.toAnimId resultId
    return $ Box.hboxAlign 0.5 [widget, label]

maybeAddExtraSymbol :: MonadA m => Bool -> Widget.Id -> Widget f -> ExprGuiM m (Widget f)
maybeAddExtraSymbol haveExtraResults myId w
  | haveExtraResults = do
    extraSymbolLabel <-
      fmap (Widget.scale extraSymbolScaleFactor) .
      ExprGuiM.widgetEnv .
      BWidgets.makeLabel extraSymbol $ Widget.toAnimId myId
    return $ BWidgets.hboxCenteredSpaced [w, extraSymbolLabel]
  | otherwise = return w

makeNoResults :: MonadA m => HoleInfo m -> AnimId -> ExprGuiM m (WidgetT m)
makeNoResults holeInfo myId =
  case hiMArgument holeInfo ^? Lens._Just . Sugar.haExpr of
  Nothing -> label "(No results)"
  Just arg ->
    Box.hboxCentered <$> sequenceA
    [ label "(No results: "
    , ExprGuiM.makeSubexpresion 0 arg
      <&> Widget.doesntTakeFocus . (^. ExpressionGui.egWidget)
    , label ")"
    ]
  where
    label str =
      ExprGuiM.widgetEnv $ BWidgets.makeLabel str myId

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

holeResultAnimMappingNoParens :: HoleInfo m -> Widget.Id -> AnimId -> AnimId
holeResultAnimMappingNoParens holeInfo resultId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) myId .
  renamePrefix myId ("old hole" : myId)
  where
    myId = Widget.toAnimId $ hiId holeInfo

hiSearchTermId :: HoleInfo m -> Widget.Id
hiSearchTermId holeInfo = WidgetIds.searchTermId $ hiId holeInfo

mkAddNewDefinitionEventMap ::
  MonadA m => HoleInfo m -> ExprGuiM m (Widget.EventHandlers (T m))
mkAddNewDefinitionEventMap holeInfo = do
  savePosition <- ExprGuiM.mkPrejumpPosSaver
  mDefRef <-
    ExprGuiM.liftMemoT $
    hiActions holeInfo ^. Sugar.holeResult $
    Sugar.ResultSeedNewDefinition newName
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    f defRef =
      E.keyPresses (Config.newDefinitionKeys config)
      (E.Doc ["Edit", "Result", "As new Definition"]) $ do
        mTargetGuid <- HoleResults.pick holeInfo defRef
        when (isJust mTargetGuid) savePosition
        pure Widget.EventResult
          { Widget._eCursor = WidgetIds.fromGuid <$> mTargetGuid
          , Widget._eAnimIdMapping =
            holeResultAnimMappingNoParens holeInfo $ hiSearchTermId holeInfo
          }
  pure $ maybe mempty f mDefRef
  where
    searchTerm = hiState holeInfo ^. Property.pVal . hsSearchTerm
    newName = concat . words $ searchTerm

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
    [] -> makeNoResults holeInfo $ Widget.toAnimId myId
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
    myId = hiId holeInfo

charGroupHandler ::
  Functor f =>
  String -> E.InputDoc -> E.Doc ->
  (Char -> f Widget.Id) ->
  Widget.EventHandlers f
charGroupHandler chars idoc doc handleChar =
  fmap Widget.eventResultFromCursor <$>
  E.charGroup idoc doc chars handler
  where
    handler char _isShifted = handleChar char

opPickEventMap ::
  MonadA m =>
  HoleInfo m -> Bool -> Sugar.HoleResult Sugar.Name m ->
  Widget.EventHandlers (T m)
opPickEventMap holeInfo isSelectedResult result
  | ignoreSearchTerm || nonEmptyAll (`notElem` operatorChars) searchTerm =
    charGroupHandler operatorChars "Operator"
    (E.Doc ["Edit", "Result", "Pick and apply operator"]) $ \c -> do
      dest <- result ^. Sugar.holeResultPickWrapped
      setHoleStateAndJump (HoleState [c]) dest
  | ignoreSearchTerm || nonEmptyAll (`elem` operatorChars) searchTerm =
    charGroupHandler alphaNumericChars "Letter/digit"
    (E.Doc ["Edit", "Result", "Pick and resume"]) $ \c -> do
      mTarget <- HoleResults.pick holeInfo result
      case mTarget of
        Nothing -> pure . WidgetIds.fromGuid $ hiGuid holeInfo
        Just targetGuid -> setHoleStateAndJump (HoleState [c]) targetGuid
  | otherwise = mempty
  where
    ignoreSearchTerm = isSelectedResult && null searchTerm
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm

mkEventMap ::
  MonadA m => HoleInfo m -> Bool ->
  Maybe (Sugar.HoleResult Sugar.Name m) ->
  ExprGuiM m (Widget.EventHandlers (T m))
mkEventMap holeInfo isSelectedResult mResult = do
  mDeleteWrapper <-
    -- TODO: DeleteWrapper is actually "unwrap" and should be exposed
    -- as a Sugar action on wrap-hole
    ExprGuiM.liftMemoT . fmap join . sequenceA $ do
      guard $ null searchTerm
      arg <- hiMArgument holeInfo
      Just . (hiActions holeInfo ^. Sugar.holeResult) .
        Sugar.ResultSeedExpression $ arg ^. Sugar.haExprPresugared
  addNewDefinitionEventMap <- mkAddNewDefinitionEventMap holeInfo
  config <- ExprGuiM.widgetEnv WE.readConfig
  pure $ mconcat
    [ addNewDefinitionEventMap
    , maybe mempty (opPickEventMap holeInfo isSelectedResult) mResult
    , maybe mempty
      ( E.keyPresses (Config.delKeys config)
        (E.Doc ["Edit", "Back"])
      . fmap (handlePickResultTargetGuid holeInfo)
      . HoleResults.pick holeInfo
      ) mDeleteWrapper
    , maybe mempty
      ( E.keyPresses (Config.delKeys config)
        (E.Doc ["Edit", "Delete"])
      . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
      ) $ do
        guard $ null searchTerm
        actions ^. Sugar.holeMDelete
    ]
  where
    actions = hiActions holeInfo
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
      | otherwise = hiId holeInfo
    destId
      | null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
      | otherwise = head (shownResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

holeBackgroundColor :: Config -> Sugar.HoleArg m expr -> Draw.Color
holeBackgroundColor config holeArg
  | holeArg ^. Sugar.haTypeIsAMatch = Config.deletableHoleBackgroundColor config
  | otherwise = Config.typeErrorHoleWrapBackgroundColor config

makeActiveHoleEdit :: MonadA m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (shownResults, hasHiddenResults) <-
    HoleResults.makeAll config holeInfo MakeWidgets
    { mkNewTagResultWidget = makeNewTagResultWidget holeInfo
    , mkResultWidget = makeHoleResultWidget holeInfo
    }
  let
    shownResultsIds = rId . (^. HoleResults.rlMain) <$> shownResults
    allResultIds = [rId . (^. HoleResults.rlMain), (^. HoleResults.rlExtraResultsPrefixId)] <*> shownResults
  assignHoleEditCursor
    holeInfo shownResultsIds allResultIds (hiSearchTermId holeInfo) $ do
      (mSelectedResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResults hasHiddenResults
      let
        mFirstResult = rHoleResult . (^. HoleResults.rlMain) <$> listToMaybe shownResults
        mResult = mSelectedResult <|> mFirstResult
        searchTermEventMap = maybe mempty (resultPickEventMap config holeInfo) mResult
      searchTermWidget <- makeSearchTermWidget holeInfo
        -- TODO: Move the result picking events into pickEventMap
        -- instead of here on the searchTerm and on each result
        & Lens.mapped . ExpressionGui.egWidget %~ Widget.strongerEvents searchTermEventMap
      holeEventMap <- mkEventMap holeInfo (isJust mSelectedResult) mResult
      case mResult of
        Nothing -> return ()
        Just res -> ExprGuiM.addResultPicker $ res ^. Sugar.holeResultPickPrefix
      let adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
      return .
        (ExpressionGui.egWidget %~
         Widget.strongerEvents holeEventMap .
         makeBackground (hiId holeInfo)
         Layers.activeHoleBG (Config.activeHoleBackgroundColor config)) $
        ExpressionGui.addBelow 0.5
        [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
        ]
        searchTermWidget

make ::
  MonadA m => Sugar.Hole Sugar.Name m (Sugar.ExpressionN m) ->
  Maybe Guid -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole mNextHoleGuid guid =
  ExpressionGui.wrapDelegated holeFDConfig
  FocusDelegator.Delegating $ \myId -> do
    inCollapsed <- ExprGuiM.isInCollapsedExpression
    mHoleNumber <-
      if isWritable && not inCollapsed
      then Just <$> ExprGuiM.nextHoleNumber
      else pure Nothing
    makeUnwrapped mHoleNumber hole mNextHoleGuid guid myId
  where
    isWritable = isJust $ hole ^. Sugar.holeMActions

makeUnwrapped ::
  MonadA m => Maybe ExprGuiM.HoleNumber ->
  Sugar.Hole Sugar.Name m (Sugar.ExpressionN m) ->
  Maybe Guid -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped mHoleNumber hole mNextHoleGuid guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  case (hole ^. Sugar.holeMActions, Widget.subId myId cursor) of
    (Just holeActions, Just _) -> do
      stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
      makeActiveHoleEdit HoleInfo
        { hiGuid = guid
        , hiId = myId
        , hiState = stateProp
        , hiActions = holeActions
        , hiMNextHoleGuid = mNextHoleGuid
        , hiMArgument = hole ^. Sugar.holeMArg
        }
    _ -> makeInactive mHoleNumber hole myId

searchTermWIdOfHoleGuid :: Guid -> Widget.Id
searchTermWIdOfHoleGuid = WidgetIds.searchTermId . FocusDelegator.delegatingId . WidgetIds.fromGuid

setHoleStateAndJump :: MonadA m => HoleState -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  pure $ searchTermWIdOfHoleGuid newHoleGuid

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
disallowChars =
  E.filterSChars (curry (`notElem` disallowedHoleChars)) .
  E.deleteKey (keyPress E.KeySpace) .
  E.deleteKey (keyPress E.KeyEnter)
  where
    keyPress = E.KeyEvent E.Press . E.ModKey E.noMods

makeSearchTermWidget ::
  MonadA m => HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv $
    (ExpressionGui.scaleFromTop (realToFrac <$> Config.holeSearchTermScaleFactor config) .
     ExpressionGui.fromValueWidget .
     (Widget.wEventMap %~ disallowChars) .
     Widget.atEvents setter) <$>
    BWidgets.makeTextEdit searchTerm (hiSearchTermId holeInfo)
  where
    searchTermProp = searchTermProperty holeInfo
    searchTerm = Property.value searchTermProp
    setter (newSearchTerm, eventRes) = do
      when (newSearchTerm /= searchTerm) $ Property.set searchTermProp newSearchTerm
      return $
        eventRes &
        -- When first letter is typed in search term, jump to the
        -- results, which will go to first result:
        if null searchTerm && (not . null) newSearchTerm
        then Widget.eCursor .~ Just (HoleResults.prefixId holeInfo)
        else id

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
makeInactive mHoleNumber hole myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  holeGui <-
    case hole ^? Sugar.holeMArg . Lens._Just . Sugar.haExpr of
    Just arg ->
      ExprGuiM.makeSubexpresion 0 arg
      -- Override "Leave Expression" of sub expression
      -- so that we don't get the inner expression wrapped again.
      -- TODO: Instead replace just the navigation to whole arg.
      & Lens.mapped . ExpressionGui.egWidget %~
        Widget.strongerEvents
        ( Widget.keysEventMapMovesCursor
          (Config.leaveSubexpressionKeys config)
          (E.Doc ["Navigation", "Leave to outer hole"])
          (return myId)
        )
    Nothing -> makeJumpableSpace mHoleNumber myId
  let
    bgColor =
      fromMaybe (Config.inactiveHoleBackgroundColor config) $
      holeBackgroundColor config <$> hole ^. Sugar.holeMArg
  ExprGuiM.widgetEnv $
    holeGui
    & ExpressionGui.egWidget %~ makeBackground myId Layers.inactiveHole bgColor
    & ExpressionGui.egWidget %%~
      if holeGui ^. ExpressionGui.egWidget . Widget.wIsFocused
      then return
      else BWidgets.makeFocusableView myId

makeJumpableSpace ::
  MonadA m => Maybe ExprGuiM.HoleNumber ->
  Widget.Id -> ExprGuiM m (ExpressionGui m1)
makeJumpableSpace mHoleNumber myId = do
  space <-
    ExprGuiM.widgetEnv .
    BWidgets.makeTextViewWidget "  " $ Widget.toAnimId myId
  savePosition <- ExprGuiM.mkPrejumpPosSaver
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    mHolePair holeNumber = do
      keys <- keysOfNum holeNumber
      let
        doc = E.Doc ["Navigation", "Jump to", "Hole " ++ show holeNumber]
        eventMap =
          Widget.keysEventMapMovesCursor keys doc $
          myId <$ savePosition
      pure ("Alt-" ++ show holeNumber, eventMap)
    mkHoleNumView (shownStr, eventMap) = do
      ExprGuiM.appendToTopLevelEventMap eventMap
      fmap (^. View.scaled (realToFrac <$> Config.holeNumLabelScaleFactor config)) .
        ExprGuiM.withFgColor (Config.holeNumLabelColor config) .
        ExprGuiM.widgetEnv .
        BWidgets.makeTextView shownStr $
        Widget.toAnimId myId ++ ["hole number"]
  mHoleNumView <- traverse mkHoleNumView $ mHolePair =<< mHoleNumber
  pure . ExpressionGui.fromValueWidget $
    maybe id Widget.overlayView mHoleNumView space
