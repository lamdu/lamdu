{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
  ( make
  ) where

import           Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, msum, when)
import           Control.MonadA (MonadA)
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import           Data.Maybe (isJust, maybeToList, fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Data.Traversable (traverse)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open.EventMap as OpenEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open.ShownResult (PickedResult(..), ShownResult(..), pickedEventResult)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Results (ResultsList(..), Result(..), HaveHiddenResults(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

compose :: [a -> a] -> a -> a
compose = foldr (.) id

eventResultOfPickedResult :: Sugar.PickedResult -> PickedResult
eventResultOfPickedResult pr =
  PickedResult
  { _pickedInnerHoleGuid = pr ^? Sugar.prMJumpTo . Lens._Just . Lens._1
  , _pickedEventResult =
    Widget.EventResult
    { Widget._eCursor =
      Monoid.Last $
      WidgetIds.fromEntityId <$> pr ^? Sugar.prMJumpTo . Lens._Just . Lens._2
    , Widget._eAnimIdMapping =
      Monoid.Endo $ pickedResultAnimIdTranslation $ pr ^. Sugar.prIdTranslation
    }
  , _pickedIdTranslations =
    pr ^. Sugar.prIdTranslation
    & Lens.mapped . Lens.both %~ WidgetIds.fromEntityId
    & mapPrefix
  }
  where
    mapPrefix = compose . map reprefix
    reprefix (old, new) ident =
      maybe ident (WidgetId.joinId new) $ WidgetId.subId old ident
    pickedResultAnimIdTranslation idTranslations =
      -- Map only the first anim id component
      Lens.ix 0 %~ \x -> fromMaybe x $ Map.lookup x idMap
      where
        idMap =
          idTranslations
          & Lens.traversed . Lens.both %~ head . Widget.toAnimId . WidgetIds.fromEntityId
          & Map.fromList

resultSuffix :: Lens.Prism' AnimId AnimId
resultSuffix = suffixed ["result suffix"]

afterPick ::
  Monad m => HoleInfo m -> Widget.Id -> Sugar.PickedResult -> T m PickedResult
afterPick holeInfo resultId pr = do
  Property.set (hiState holeInfo) HoleState.emptyState
  eventResultOfPickedResult pr
    & pickedEventResult . Widget.eCursor %~
      mappend (Monoid.Last (Just myHoleId))
    & pickedEventResult . Widget.eAnimIdMapping %~
      mappend (Monoid.Endo obliterateOtherResults)
    & return
  where
    myHoleId = WidgetIds.fromEntityId $ hiEntityId holeInfo
    obliterateOtherResults animId =
      case animId ^? resultSuffix of
      Nothing -> animId
      Just unsuffixed
        | Lens.has (suffixed (Widget.toAnimId resultId)) unsuffixed -> animId
        | otherwise -> "obliterated" : animId

makeShownResult ::
  MonadA m => HoleInfo m -> Result m -> ExprGuiM m (Widget (T m), ShownResult m)
makeShownResult holeInfo result =
  do
    -- Warning: rHoleResult should be ran at most once!
    -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
    res <- ExprGuiM.transaction $ rHoleResult result
    config <- Config.hole <$> ExprGuiM.widgetEnv WE.readConfig
    (widget, eventMap) <- makeHoleResultWidget (rId result) res
    return
      ( widget & (Widget.pad . fmap realToFrac . Config.holeResultPadding) config
      , ShownResult
        { srEventMap = eventMap
        , srHoleResult = res
        , srPick = afterPick holeInfo (rId result) =<< res ^. Sugar.holeResultPick
        }
      )

makeExtraSymbolWidget :: MonadA m => AnimId -> Bool -> ResultsList n -> ExprGuiM m (Widget f)
makeExtraSymbolWidget animId isSelected results
  | Lens.nullOf (HoleResults.rlExtra . traverse) results = pure Spacer.empty
  | otherwise =
    do
      Config.Hole{..} <- Config.hole <$> ExprGuiM.widgetEnv WE.readConfig
      let
        extraSymbolColor
          | isSelected = holeExtraSymbolColorSelected
          | otherwise = holeExtraSymbolColorUnselected
      ExprGuiM.makeLabel extraSymbol animId
        <&> Widget.scale extraSymbolScaleFactor
        <&> Widget.tint extraSymbolColor
        >>= ExprGuiM.widgetEnv . BWidgets.hboxCenteredSpaced . (Spacer.empty :) . (: [])

data ResultGroupWidgets m = ResultGroupWidgets
  { _rgwMainResult :: ShownResult m
  , _rgwMSelectedResult :: Maybe (ShownResult m) -- Can be an extra result
  , _rgwRow :: [WidgetT m]
  }
rgwMainResult :: Lens' (ResultGroupWidgets m) (ShownResult m)
rgwMainResult f ResultGroupWidgets{..} = f _rgwMainResult <&> \_rgwMainResult -> ResultGroupWidgets{..}
rgwMSelectedResult :: Lens' (ResultGroupWidgets m) (Maybe (ShownResult m))
rgwMSelectedResult f ResultGroupWidgets{..} = f _rgwMSelectedResult <&> \_rgwMSelectedResult -> ResultGroupWidgets{..}
rgwRow :: Lens' (ResultGroupWidgets m) [WidgetT m]
rgwRow f ResultGroupWidgets{..} = f _rgwRow <&> \_rgwRow -> ResultGroupWidgets{..}

makeResultGroup ::
  MonadA m =>
  HoleInfo m ->
  ResultsList m ->
  ExprGuiM m (ResultGroupWidgets m)
makeResultGroup holeInfo results = do
  Config.Hole{..} <- Config.hole <$> ExprGuiM.widgetEnv WE.readConfig
  (mainResultWidget, shownMainResult) <- makeShownResult holeInfo mainResult
  let
    mainResultHeight = mainResultWidget ^. Widget.wSize . Lens._2
    makeExtra = makeExtraResultsWidget holeInfo mainResultHeight $ results ^. HoleResults.rlExtra
  (mSelectedResult, extraResWidget) <-
    if mainResultWidget ^. Widget.wIsFocused
    then do
      widget <- snd <$> makeExtra
      return (Just shownMainResult, widget)
    else do
      cursorOnExtra <-
        ExprGuiM.widgetEnv . WE.isSubCursor $ results ^. HoleResults.rlExtraResultsPrefixId
      if cursorOnExtra
        then makeExtra
        else
          (,) Nothing <$>
          makeExtraResultsPlaceholderWidget (results ^. HoleResults.rlExtra)
  let isSelected = Lens.has Lens._Just mSelectedResult
  extraSymbolWidget <- makeExtraSymbolWidget (Widget.toAnimId (rId mainResult)) isSelected results
  return ResultGroupWidgets
    { _rgwMainResult = shownMainResult
    , _rgwMSelectedResult = mSelectedResult
    , _rgwRow = [mainResultWidget, extraSymbolWidget, extraResWidget]
    }
  where
    mainResult = results ^. HoleResults.rlMain

makeExtraResultsPlaceholderWidget ::
  MonadA m => [Result m] -> ExprGuiM m (WidgetT m)
makeExtraResultsPlaceholderWidget [] = return Spacer.empty
makeExtraResultsPlaceholderWidget (result:_) =
  makeFocusable (rId result) Spacer.empty

makeExtraResultsWidget ::
  MonadA m => HoleInfo m -> Anim.R -> [Result m] ->
  ExprGuiM m (Maybe (ShownResult m), WidgetT m)
makeExtraResultsWidget _ _ [] = return (Nothing, Spacer.empty)
makeExtraResultsWidget holeInfo mainResultHeight extraResults@(firstResult:_) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    Config.Hole{..} = Config.hole config
    mkResWidget result = do
      isOnResult <- ExprGuiM.widgetEnv $ WE.isSubCursor (rId result)
      (widget, shownResult) <- makeShownResult holeInfo result
      return
        ( shownResult <$ guard isOnResult
        , widget
        )
  (mResults, widgets) <-
    unzip <$> traverse mkResWidget extraResults
  let
    headHeight = head widgets ^. Widget.wSize . Lens._2
    height = min mainResultHeight headHeight
  return
    ( msum mResults
    , Box.vboxAlign 0 widgets
      & addBackground (rId firstResult) (Config.layers config) holeOpenBGColor
      & Widget.wSize .~ Vector2 0 height
      & Widget.translate (Vector2 0 (0.5 * (height - headHeight)))
    )

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId

makeHoleResultWidget ::
  MonadA m => Widget.Id ->
  Sugar.HoleResult (Name m) m -> ExprGuiM m (WidgetT m, Widget.EventHandlers (T m))
makeHoleResultWidget resultId holeResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  eventMap <-
    do
      -- Create a hidden result widget that we never display, but only
      -- keep the event map from. We always tell it that it has focus,
      -- so that even if we're on the search term, we can have valid
      -- event maps of any result (we actually use the first one's
      -- event map)
      hiddenResultWidget <- mkWidget & ExprGuiM.localEnv (WE.envCursor .~ idWithinResultWidget)
      return $ hiddenResultWidget ^. Widget.wEventMap
  widget <-
    mkWidget
    <&> Widget.wFrame %~ Anim.mapIdentities (<> (resultSuffix # Widget.toAnimId resultId))
    <&> Widget.scale (realToFrac <$> holeResultScaleFactor)
    <&> Widget.wEventMap .~ mempty
    >>= makeFocusable resultId
    <&> BWidgets.liftLayerInterval config
  return (widget, eventMap)
  where
    mkWidget =
      holeResultConverted
      & postProcessSugar
      & ExprGuiM.makeSubexpression 0
      <&> (^. ExpressionGui.egWidget)
    holeResultEntityId = holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
    idWithinResultWidget =
      WidgetIds.fromEntityId $ fromMaybe holeResultEntityId $
      holeResult ^. Sugar.holeResultHoleTarget
    holeResultConverted = holeResult ^. Sugar.holeResultConverted

postProcessSugar ::
  MonadA m =>
  ExpressionN m Sugar.IsInjected ->
  ExpressionN m ExprGuiM.Payload
postProcessSugar expr =
  expr
  <&> Lens.mapped %~ toPayload
  & SugarLens.holeArgs . Sugar.plData . ExprGuiM.plShowType .~ ExprGuiM.ShowType

toPayload :: Sugar.IsInjected -> ExprGuiM.Payload
toPayload isInjected =
  ExprGuiM.emptyPayload NearestHoles.none
  & ExprGuiM.plShowType .~ ExprGuiM.DoNotShowType
  & ExprGuiM.plInjected .~
    case isInjected of
    Sugar.NotInjected -> []
    Sugar.Injected -> [True]

makeNoResults :: MonadA m => AnimId -> ExprGuiM m (WidgetT m)
makeNoResults animId =
  ExpressionGui.makeLabel "(No results)" animId
  <&> (^. ExpressionGui.egWidget)

hiSearchTermId :: HoleInfo m -> Widget.Id
hiSearchTermId = WidgetIds.searchTermId . hidOpen . hiIds

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
  Just <$> ExprGuiM.makeLabel "..." (Widget.toAnimId myId)
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

addMResultPicker :: MonadA m => Maybe (ShownResult m) -> ExprGuiM m ()
addMResultPicker mSelectedResult =
  case mSelectedResult of
    Nothing -> return ()
    Just res -> ExprGuiM.addResultPicker $ (^. pickedEventResult) <$> srPick res

layoutResults ::
  MonadA m =>
  [[Widget (T m)]] -> HaveHiddenResults -> Widget.Id -> ExprGuiM m (WidgetT m)
layoutResults rows hiddenResults myId
  | null rows = makeNoResults (Widget.toAnimId myId)
  | otherwise =
    do
      hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
      let grid =
            rows
            & Lens.mapped . Lens.mapped %~ (,) (Vector2 0 0.5)
            & Grid.make & Grid.toWidget
            & OpenEventMap.blockDownEvents
      return $ Box.vboxCentered $ grid : hiddenResultsWidgets

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveHiddenResults ->
  ExprGuiM m (Maybe (ShownResult m), WidgetT m)
makeResultsWidget holeInfo shownResultsLists hiddenResults = do
  groupsWidgets <- traverse (makeResultGroup holeInfo) shownResultsLists
  let
    mSelectedResult = groupsWidgets ^? Lens.traversed . rgwMSelectedResult . Lens._Just
    mFirstResult = groupsWidgets ^? Lens.traversed . rgwMainResult
    mResult = mSelectedResult <|> mFirstResult
    rows = groupsWidgets ^.. Lens.traversed . rgwRow
  addMResultPicker mResult
  widget <- layoutResults rows hiddenResults myId
  return (mResult, widget)
  where
    myId = hidOpen (hiIds holeInfo)

assignHoleEditCursor ::
  MonadA m =>
  HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
  ExprGuiM m a ->
  ExprGuiM m a
assignHoleEditCursor holeInfo shownMainResultsIds allShownResultIds searchTermId action = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ HoleResults.prefixId holeInfo
    isOnResult = any sub allShownResultIds
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hidOpen (hiIds holeInfo)
    destId
      | null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
      | otherwise = head (shownMainResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

maybeHoverClosedHoleAbove ::
  MonadA m =>
  HoleInfo n -> ExpressionGui m ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
maybeHoverClosedHoleAbove holeInfo closedHoleGui openHoleGui
  | Lens.has Lens._Just (hiMArgument holeInfo) =
    do
      Config.Hole{..} <- ExprGuiM.widgetEnv WE.readConfig <&> Config.hole
      hoveringClosedHole <-
        addDarkBackground (hidClosed (hiIds holeInfo)) closedHoleGui
        <&> (^. ExpressionGui.egWidget)
        <&> Widget.scale (holeHoveringWrapperScale <&> realToFrac)
      ExpressionGui.addAbove 0
        [(0, hoveringClosedHole)] openHoleGui
        & return
  | otherwise = return openHoleGui

make ::
  MonadA m =>
  ExpressionGui m ->
  Sugar.Payload m ExprGuiM.Payload -> HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
make closedHoleGui pl holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  (shownResultsLists, hasHiddenResults) <-
    -- Don't generate results of open holes inside hole results
    if isHoleResult
    then return ([], HaveHiddenResults)
    else HoleResults.makeAll (Config.hole config) holeInfo
  let
    shownMainResultsIds = rId . (^. HoleResults.rlMain) <$> shownResultsLists
    allShownResultIds = [rId . (^. HoleResults.rlMain), (^. HoleResults.rlExtraResultsPrefixId)] <*> shownResultsLists
  assignHoleEditCursor
    holeInfo shownMainResultsIds allShownResultIds (hiSearchTermId holeInfo) $ do
      (mShownResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResultsLists hasHiddenResults
      (searchTermEventMap, resultsEventMap) <- OpenEventMap.make holeInfo mShownResult
      rawOpenHole <-
        makeSearchTermGui holeInfo
        <&> ExpressionGui.egWidget %~
            Widget.weakerEvents searchTermEventMap
        <&> ExpressionGui.addBelow 0.5
            [(0.5, Widget.strongerEvents resultsEventMap resultsWidget)]
      -- We make our own type view here instead of
      -- ExpressionGui.stdWrap, because we want to synchronize the
      -- active BG width with the inferred type width
      typeView <-
        ExpressionGui.makeTypeView (rawOpenHole ^. guiWidth)
        (pl ^. Sugar.plEntityId) (pl ^. Sugar.plInferredType)
      rawOpenHole
        & guiWidth %~ max (typeView ^. Widget.wSize . _1)
        & ExpressionGui.egWidget %~
          addBackground (hidOpen (hiIds holeInfo))
          (Config.layers config) holeOpenBGColor
        & ExpressionGui.addBelowInferredSpacing typeView
        >>= addDarkBackground (hidOpen (hiIds holeInfo))
        & ExpressionGui.wrapExprEventMap pl
        >>= maybeHoverClosedHoleAbove holeInfo closedHoleGui
  where
    isHoleResult =
      Lens.nullOf (Sugar.plData . ExprGuiM.plStoredEntityIds . Lens.traversed) pl

guiWidth :: Lens' (ExpressionGui m) Widget.R
guiWidth = ExpressionGui.egWidget . Widget.wSize . _1

addDarkBackground :: MonadA m => Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)
addDarkBackground myId widget =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let Config.Hole{..} = Config.hole config
    widget
      & ExpressionGui.pad (holeOpenDarkPadding <&> realToFrac)
      & ExpressionGui.egWidget %~
        Widget.backgroundColor
        (Config.layerDarkOpenHoleBG (Config.layers config))
        (Widget.toAnimId myId <> ["hole dark background"])
        holeOpenDarkBGColor
      & return

makeSearchTermGui ::
  MonadA m => HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermGui holeInfo = do
  Config.Hole{..} <- Config.hole <$> ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv $
    (ExpressionGui.scaleFromTop (realToFrac <$> holeSearchTermScaleFactor) .
     ExpressionGui.fromValueWidget .
     (Widget.wEventMap %~ OpenEventMap.disallowChars searchTerm) .
     Widget.atEvents setter) <$>
    BWidgets.makeTextEdit searchTerm (hiSearchTermId holeInfo)
  where
    searchTermProp = HoleInfo.hiSearchTermProperty holeInfo
    searchTerm = Property.value searchTermProp
    setter (newSearchTerm, eventRes) = do
      when (newSearchTerm /= searchTerm) $ Property.set searchTermProp newSearchTerm
      return $
        eventRes &
        -- When first letter is typed in search term, jump to the
        -- results, which will go to first result:
        if null searchTerm && (not . null) newSearchTerm
        then Widget.eCursor .~ (Monoid.Last . Just . HoleResults.prefixId) holeInfo
        else id
