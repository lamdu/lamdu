{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make, makeUnwrappedActive
  , HoleState(..), hsSearchTerm
  , setHoleStateAndJump
  , pickedResultAnimIdTranslation
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Lens.Operators
import Control.Monad (msum, when)
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Maybe (isJust, listToMaybe, maybeToList, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CharClassification (operatorChars, alphaNumericChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleState(..), hsSearchTerm)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Results (MakeWidgets(..), ResultsList(..), Result(..), HaveHiddenResults(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Sugar.Types as Sugar

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

type T = Transaction

pickedResultAnimIdTranslation ::
  [(Guid, Guid)] ->
  AnimId -> AnimId
pickedResultAnimIdTranslation idTranslations =
  -- Map only the first anim id component
  Lens.ix 0 %~ \x -> fromMaybe x $ Map.lookup x idMap
  where
    idMap =
      idTranslations
      & Lens.traversed . Lens.both %~ head . Widget.toAnimId . WidgetIds.fromGuid
      & Map.fromList

pick :: Monad m => HoleInfo m -> Sugar.PickedResult -> T m Widget.EventResult
pick holeInfo pr = do
  Property.set (hiState holeInfo) HoleInfo.emptyState
  return
    Widget.EventResult
    { Widget._eCursor =
        Just . WidgetIds.fromGuid $
        fromMaybe (hiGuid holeInfo) (pr ^. Sugar.prMJumpTo)
    , Widget._eAnimIdMapping =
        pickedResultAnimIdTranslation (pr ^. Sugar.prIdTranslation)
    }

pickAndSetNextHoleState ::
  MonadA m =>
  HoleInfo m -> String -> Sugar.PickedResult -> T m Widget.EventResult
pickAndSetNextHoleState holeInfo searchTerm pr =
  pick holeInfo pr <*
  case pr ^. Sugar.prMJumpTo of
    Just newHoleGuid ->
      Transaction.setP (assocStateRef newHoleGuid) $ HoleState searchTerm
    Nothing -> return ()

resultPickEventMap ::
  MonadA m => Config -> HoleInfo m -> Sugar.HoleResult Sugar.Name m ExprGuiM.Payload ->
  Widget.EventHandlers (T m)
resultPickEventMap config holeInfo holeResult =
  case hiMNextHoleGuid holeInfo of
  Just nextHoleGuid
    | not (holeResult ^. Sugar.holeResultHasHoles) ->
      mappend (simplePickRes (Config.pickResultKeys config)) .
      E.keyPresses (Config.pickAndMoveToNextHoleKeys config)
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) $
        (Widget.eCursor .~ Just (WidgetIds.fromGuid nextHoleGuid)) <$>
        (pick holeInfo =<< holeResult ^. Sugar.holeResultPick)
  _ ->
    simplePickRes $
    Config.pickResultKeys config ++
    Config.pickAndMoveToNextHoleKeys config
  where
    simplePickRes keys =
      E.keyPresses keys (E.Doc ["Edit", "Result", "Pick"]) $
      pick holeInfo =<< holeResult ^. Sugar.holeResultPick

makeResultGroup ::
  MonadA m =>
  ResultsList m ->
  ExprGuiM m
  ( [WidgetT m]
  , Maybe (Sugar.HoleResult Sugar.Name m ExprGuiM.Payload)
  )
makeResultGroup results = do
  mainResultWidget <- rMkWidget mainResult
  extraSymbolWidget <-
    if Lens.has (HoleResults.rlExtra . traverse) results
    then
      BWidgets.hboxCenteredSpaced . (Spacer.empty :) . (: []) .
      Widget.scale extraSymbolScaleFactor
      <$>
      ExprGuiM.widgetEnv
      (BWidgets.makeLabel extraSymbol (Widget.toAnimId (rId mainResult)))
    else pure Spacer.empty
  (mResult, extraResWidget) <-
    if mainResultWidget ^. Widget.wIsFocused
    then do
      widget <- snd <$> makeExtra
      return (Just (rHoleResult (results ^. HoleResults.rlMain)), widget)
    else do
      cursorOnExtra <-
        ExprGuiM.widgetEnv . WE.isSubCursor $ results ^. HoleResults.rlExtraResultsPrefixId
      if cursorOnExtra
        then makeExtra
        else
          (,) Nothing <$>
          makeExtraResultsPlaceholderWidget (results ^. HoleResults.rlExtra)
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    onExtraSymbol =
      case mResult of
      Nothing -> Widget.tint $ Config.holeInactiveExtraSymbolColor config
      Just _ -> id
  return ([mainResultWidget, onExtraSymbol extraSymbolWidget, extraResWidget], mResult)
  where
    mainResult = results ^. HoleResults.rlMain
    makeExtra = makeExtraResultsWidget $ results ^. HoleResults.rlExtra

makeExtraResultsPlaceholderWidget ::
  MonadA m => [Result m] -> ExprGuiM m (WidgetT m)
makeExtraResultsPlaceholderWidget [] = return Spacer.empty
makeExtraResultsPlaceholderWidget (result:_) =
  ExprGuiM.widgetEnv $
  BWidgets.makeFocusableView (rId result) Spacer.empty

makeExtraResultsWidget ::
  MonadA m => [Result m] ->
  ExprGuiM m (Maybe (Sugar.HoleResult Sugar.Name m ExprGuiM.Payload), WidgetT m)
makeExtraResultsWidget [] = return (Nothing, Spacer.empty)
makeExtraResultsWidget extraResults@(firstResult:_) = do
  (mResults, widgets) <-
    unzip <$> traverse mkResWidget extraResults
  config <- ExprGuiM.widgetEnv WE.readConfig
  return
    ( msum mResults
    , Box.vboxAlign 0 widgets
      & makeBackground (rId firstResult)
        (Config.layerMax (Config.layers config))
        (Config.activeHoleBackgroundColor config)
      & Widget.wSize .~ (head widgets ^. Widget.wSize & Lens._1 .~ 0)
    )
  where
    mkResWidget result = do
      isOnResult <- ExprGuiM.widgetEnv $ WE.isSubCursor (rId result)
      widget <- rMkWidget result
      return (if isOnResult then Just holeResultSugar else Nothing, widget)
      where
        holeResultSugar = rHoleResult result

makeHoleResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult Sugar.Name m ExprGuiM.Payload -> ExprGuiM m (WidgetT m)
makeHoleResultWidget holeInfo resultId holeResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
    -- TODO: No need for this if we just add a pick result event map
    -- to the whole hole
    Widget.scale (realToFrac <$> Config.holeResultScaleFactor config) .
    Widget.strongerEvents (resultPickEventMap config holeInfo holeResult) .
    (Widget.wFrame %~ Anim.mapIdentities (`mappend` Widget.toAnimId resultId)) .
    (^. ExpressionGui.egWidget) =<<
    (ExprGuiM.makeSubexpression 0 . SugarRemoveTypes.holeResultTypes)
    (holeResult ^. Sugar.holeResultConverted)

asNewLabelScaleFactor :: Fractional a => a
asNewLabelScaleFactor = 0.5

makeNewTagResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult Sugar.Name m ExprGuiM.Payload ->
  ExprGuiM m (WidgetT m)
makeNewTagResultWidget holeInfo resultId holeResult = do
  widget <- makeHoleResultWidget holeInfo resultId holeResult
  ExprGuiM.widgetEnv $ do
    label <-
      fmap (Widget.scale asNewLabelScaleFactor) .
      BWidgets.makeLabel " (as new tag)" $ Widget.toAnimId resultId
    return $ Box.hboxAlign 0.5 [widget, label]

makeNoResults :: MonadA m => HoleInfo m -> AnimId -> ExprGuiM m (WidgetT m)
makeNoResults holeInfo myId =
  case hiMArgument holeInfo ^? Lens._Just . Sugar.haExpr of
  Nothing -> label "(No results)"
  Just arg ->
    Box.hboxCentered <$> sequenceA
    [ label "(No results: "
    , ExprGuiM.makeSubexpression 0 arg
      <&> Widget.doesntTakeFocus . (^. ExpressionGui.egWidget)
    , label ")"
    ]
  where
    label str =
      ExprGuiM.widgetEnv $ BWidgets.makeLabel str myId

hiSearchTermId :: HoleInfo m -> Widget.Id
hiSearchTermId holeInfo = WidgetIds.searchTermId $ hiId holeInfo

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
  fmap Just . ExprGuiM.widgetEnv . BWidgets.makeLabel "..." $
  Widget.toAnimId myId
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

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
  ExprGuiM m (Maybe (Sugar.HoleResult Sugar.Name m ExprGuiM.Payload), WidgetT m)
makeResultsWidget holeInfo shownResults hiddenResults = do
  (rows, mResults) <- unzip <$> traverse makeResultGroup shownResults
  let mResult = mResults ^? Lens.traversed . Lens._Just
  hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
  widget <-
    if null rows
    then makeNoResults holeInfo (Widget.toAnimId myId)
    else
      return .
      Box.vboxCentered $
      ( blockDownEvents . Grid.toWidget . Grid.make
      . (map . map) ((,) (Vector2 0 0.5))
      ) rows :
      hiddenResultsWidgets
  return (mResult, widget)
  where
    myId = hiId holeInfo

opPickEventMap ::
  MonadA m =>
  HoleInfo m -> Bool -> Sugar.HoleResult Sugar.Name m a ->
  Widget.EventHandlers (T m)
opPickEventMap holeInfo isSelectedResult result
  | ignoreSearchTerm || nonEmptyAll (`notElem` operatorChars) searchTerm =
    E.charGroup "Operator"
    (E.Doc ["Edit", "Result", "Pick and apply operator"])
    operatorChars $ \c _ -> do
      pickAndSetNextHoleState holeInfo [c] =<< result ^. Sugar.holeResultPickWrapped
  | ignoreSearchTerm || nonEmptyAll (`elem` operatorChars) searchTerm =
    E.charGroup "Letter/digit"
    (E.Doc ["Edit", "Result", "Pick and resume"])
    alphaNumericChars $ \c _ -> do
      pickAndSetNextHoleState holeInfo [c] =<< result ^. Sugar.holeResultPick
  | otherwise = mempty
  where
    ignoreSearchTerm = isSelectedResult && null searchTerm
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm

mkEventMap ::
  MonadA m => HoleInfo m -> Bool ->
  Maybe (Sugar.HoleResult Sugar.Name m a) ->
  Widget.EventHandlers (T m)
mkEventMap holeInfo isSelectedResult =
  maybe mempty $ opPickEventMap holeInfo isSelectedResult

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

makeActiveHoleEdit ::
  MonadA m =>
  Widget.Size ->
  Sugar.Payload Sugar.Name m a -> HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit size pl holeInfo = do
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
      case mResult of
        Nothing -> return ()
        Just res -> ExprGuiM.addResultPicker $ res ^. Sugar.holeResultPickPrefix
      let
        adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
        holeEventMap = mkEventMap holeInfo (isJust mSelectedResult) mResult
        layers = Config.layers config
        layerDiff = Config.layerActiveHoleBG layers - Config.layerMax layers
      gui <-
        ExpressionGui.addInferredTypes pl $
        ExpressionGui.addBelow 0.5
        [(0.5, Widget.strongerEvents adHocEditor resultsWidget)]
        searchTermWidget
      gui
        & ExpressionGui.truncateSize
          ( size
            & Lens._1 %~ max (gui ^. ExpressionGui.egWidget . Widget.wSize . Lens._1) ) .
          ( ExpressionGui.egWidget %~
            Widget.strongerEvents holeEventMap .
            (Widget.wFrame %~ Anim.onDepth (+ layerDiff)) .
            makeBackground (hiId holeInfo)
              (Config.layerMax (Config.layers config))
              (Config.activeHoleBackgroundColor config)
          )
        & return

data IsActive = Inactive | Active

make ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Maybe Guid -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make pl hole mNextHoleGuid guid outerId = do
  stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
  let
    delegatingMode
      | Lens.has (Sugar.holeMArg . Lens._Just) hole &&
        null (Property.value stateProp ^. hsSearchTerm) = FocusDelegator.NotDelegating
      | otherwise = FocusDelegator.Delegating
    inner myId = makeUnwrappedH stateProp pl hole mNextHoleGuid guid myId
  config <- ExprGuiM.widgetEnv WE.readConfig
  (isActive, innerGui) <-
    ExprGuiM.wrapDelegated holeFDConfig delegatingMode
    (Lens._2 . ExpressionGui.egWidget %~) inner outerId
  gui <-
    innerGui
    & case isActive of
      Inactive -> ExpressionGui.addInferredTypes pl
      Active -> return
  gui
    & ExpressionGui.egWidget %~
      Widget.weakerEvents
      (maybe mempty
        ( E.keyPresses (Config.acceptKeys config ++ Config.delKeys config)
          (E.Doc ["Edit", "Unwrap"])
        . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
        ) mUnWrap)
    & return
  where
    mUnWrap =
      hole ^? Sugar.holeMActions . Lens._Just . Sugar.holeMUnwrap . Lens._Just

makeUnwrappedH ::
  MonadA m =>
  Property (T m) HoleState ->
  Sugar.Payload Sugar.Name m a ->
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Maybe Guid -> Guid ->
  Widget.Id ->
  ExprGuiM m (IsActive, ExpressionGui m)
makeUnwrappedH stateProp pl hole mNextHoleGuid guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  inactive <- makeInactive hole myId
  case (hole ^. Sugar.holeMActions, Widget.subId myId cursor) of
    (Just holeActions, Just _) -> do
      inactiveWithTypes <- ExpressionGui.addInferredTypes pl inactive
      (,) Active <$> makeActiveHoleEdit
        (inactiveWithTypes ^. ExpressionGui.egWidget . Widget.wSize) pl
        HoleInfo
        { hiGuid = guid
        , hiId = myId
        , hiState = stateProp
        , hiActions = holeActions
        , hiMNextHoleGuid = mNextHoleGuid
        , hiMArgument = hole ^. Sugar.holeMArg
        }
    _ -> return $ (Inactive, inactive)

makeUnwrappedActive ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Sugar.HoleActions Sugar.Name m ->
  Widget.Size -> Maybe Guid -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrappedActive pl holeActions size mNextHoleGuid guid myId = do
  stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
  makeActiveHoleEdit size pl HoleInfo
    { hiGuid = guid
    , hiId = myId
    , hiState = stateProp
    , hiActions = holeActions
    , hiMNextHoleGuid = mNextHoleGuid
    , hiMArgument = Nothing
    }

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

makeInactive ::
  MonadA m =>
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactive hole myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  holeGui <-
    case hole ^? Sugar.holeMArg . Lens._Just . Sugar.haExpr of
    Just arg ->
      ExprGuiM.makeSubexpression 0 arg
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
    Nothing ->
      ExprGuiM.widgetEnv $
      ExpressionGui.fromValueWidget <$>
      BWidgets.makeTextViewWidget "  " (Widget.toAnimId myId)
  let
    bgColor =
      fromMaybe (Config.inactiveHoleBackgroundColor config) $
      holeBackgroundColor config <$> hole ^. Sugar.holeMArg
  ExprGuiM.widgetEnv $
    holeGui
    & ExpressionGui.egWidget %~
      makeBackground myId
      (Config.layerInactiveHole (Config.layers config)) bgColor
    & ExpressionGui.egWidget %%~
      if holeGui ^. ExpressionGui.egWidget . Widget.wIsFocused
      then return
      else BWidgets.makeFocusableView myId
