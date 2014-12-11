{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Lens.Operators
import Control.Monad (guard, msum, when)
import Control.MonadA (MonadA)
import Data.List.Lens (suffixed)
import Data.Maybe (isJust, maybeToList, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (makeBackground)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import Lamdu.GUI.ExpressionEdit.HoleEdit.Open.ShownResult (ShownResult(..), srPick)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Results (ResultsList(..), Result(..), HaveHiddenResults(..))
import Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open.EventMap as OpenEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

eventResultOfPickedResult :: Sugar.PickedResult -> (Maybe Guid, Widget.EventResult)
eventResultOfPickedResult pr =
  ( pr ^? Sugar.prMJumpTo . Lens._Just . Lens._1
  , Widget.EventResult
    { Widget._eCursor =
      Monoid.Last $
      WidgetIds.fromEntityId <$> pr ^? Sugar.prMJumpTo . Lens._Just . Lens._2
    , Widget._eAnimIdMapping =
      Monoid.Endo $ pickedResultAnimIdTranslation (pr ^. Sugar.prIdTranslation)
    }
  )
  where
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
  Monad m => HoleInfo m -> Widget.Id -> Sugar.PickedResult ->
  T m (Maybe Guid, Widget.EventResult)
afterPick holeInfo resultId pr = do
  Property.set (hiState holeInfo) HoleState.emptyState
  eventResultOfPickedResult pr
    & Lens._2 . Widget.eCursor %~
      (mappend . Monoid.Last . Just .
       WidgetIds.fromEntityId . hiEntityId) holeInfo
    & Lens._2 . Widget.eAnimIdMapping %~
      (mappend . Monoid.Endo) obliterateOtherResults
    & return
  where
    obliterateOtherResults animId =
      case animId ^? resultSuffix of
      Nothing -> animId
      Just unsuffixed
        | Lens.has (suffixed (Widget.toAnimId resultId)) unsuffixed -> animId
        | otherwise -> "obliterated" : animId

makePaddedResult :: MonadA m => Result m -> ExprGuiM m (WidgetT m)
makePaddedResult res = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  rHoleResult res
    & ExprGuiM.transaction
    >>= makeHoleResultWidget (rId res)
    <&> (Widget.pad . fmap realToFrac . Config.holeResultPadding) config

makeShownResult ::
  MonadA m => HoleInfo m -> Result m -> ExprGuiM m (Widget (T m), ShownResult m)
makeShownResult holeInfo result = do
  widget <- makePaddedResult result
  res <- ExprGuiM.transaction $ rHoleResult result
  return
    ( widget & Widget.wEventMap .~ mempty
    , ShownResult
      { srEventMap = widget ^. Widget.wEventMap
      , srHoleResult = res
      , srPickTo =
        afterPick holeInfo (rId result) =<< res ^. Sugar.holeResultPick
      }
    )

makeResultGroup ::
  MonadA m =>
  HoleInfo m ->
  ResultsList m ->
  ExprGuiM m
  ( ShownResult m
  , [WidgetT m]
  , Maybe (ShownResult m)
  )
makeResultGroup holeInfo results = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (mainResultWidget, shownMainResult) <- makeShownResult holeInfo mainResult
  extraSymbolWidget <-
    if Lens.has (HoleResults.rlExtra . traverse) results
    then
      BWidgets.hboxCenteredSpaced . (Spacer.empty :) . (: []) .
      Widget.scale extraSymbolScaleFactor <$>
      ExprGuiM.widgetEnv
      (BWidgets.makeLabel extraSymbol (Widget.toAnimId (rId mainResult)))
    else pure Spacer.empty
  let
    makeExtra =
      makeExtraResultsWidget holeInfo
      (mainResultWidget ^. Widget.wSize . Lens._2)
      (results ^. HoleResults.rlExtra)
  (mResult, extraResWidget) <-
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
  let
    onExtraSymbol =
      case mResult of
      Nothing -> Widget.tint $ Config.holeInactiveExtraSymbolColor config
      Just _ -> id
  return (shownMainResult, [mainResultWidget, onExtraSymbol extraSymbolWidget, extraResWidget], mResult)
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
      & makeBackground (rId firstResult)
        (Config.layerMax (Config.layers config))
        (Config.activeHoleBGColor config)
      & Widget.wSize .~ Vector2 0 height
      & Widget.translate (Vector2 0 (0.5 * (height - headHeight)))
    )

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId

makeHoleResultWidget ::
  MonadA m => Widget.Id ->
  Sugar.HoleResult (Name m) m -> ExprGuiM m (WidgetT m)
makeHoleResultWidget resultId holeResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  resultGui <-
    ExprGuiM.makeSubexpression 0 .
    postProcessSugar $ holeResult ^. Sugar.holeResultConverted
  resultGui ^. ExpressionGui.egWidget
    & Widget.wFrame %~ Anim.mapIdentities (`mappend` (resultSuffix # Widget.toAnimId resultId))
    & Widget.scale (realToFrac <$> Config.holeResultScaleFactor config)
    & makeFocusable resultId

postProcessSugar ::
  MonadA m =>
  ExpressionN m Sugar.IsInjected ->
  ExpressionN m ExprGuiM.Payload
postProcessSugar expr =
  expr
  & Lens.mapped . Lens.mapped %~ toPayload
  -- Remove the top-level result's actions so that we can safely use
  -- the special events from the result's event map (e.g: add list
  -- item) without also getting unwanted handlers like "delete".
  & Sugar.rPayload . Sugar.plActions .~ Nothing

toPayload :: Sugar.IsInjected -> ExprGuiM.Payload
toPayload isInjected =
  ExprGuiM.Payload
  { ExprGuiM._plStoredEntityIds = []
  , ExprGuiM._plInjected =
    case isInjected of
    Sugar.NotInjected -> []
    Sugar.Injected -> [True]
  , ExprGuiM._plHoleEntityIds = ExprGuiM.emptyHoleEntityIds
  , ExprGuiM._plShowType = ExprGuiM.DoNotShowType
  }

makeNoResults :: MonadA m => HoleInfo m -> AnimId -> ExprGuiM m (WidgetT m)
makeNoResults holeInfo myId =
  (^. ExpressionGui.egWidget) <$>
  case hiMArgument holeInfo ^? Lens._Just . Sugar.haExpr of
  Nothing -> label "(No results)"
  Just arg ->
    ExpressionGui.hbox <$> sequenceA
    [ label "(No results: "
    , ExprGuiM.makeSubexpression 0 arg <&>
      ExpressionGui.egWidget %~ Widget.doesntTakeFocus
    , label ")"
    ]
  where
    label str =
      ExpressionGui.fromValueWidget <$> ExprGuiM.widgetEnv (BWidgets.makeLabel str myId)

hiSearchTermId :: HoleInfo m -> Widget.Id
hiSearchTermId holeInfo = WidgetIds.searchTermId $ HoleInfo.hiActiveId holeInfo

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
  fmap Just . ExprGuiM.widgetEnv . BWidgets.makeLabel "..." $
  Widget.toAnimId myId
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

addMResultPicker :: MonadA m => Maybe (ShownResult m) -> ExprGuiM m ()
addMResultPicker mSelectedResult =
  case mSelectedResult of
    Nothing -> return ()
    Just res -> ExprGuiM.addResultPicker $ srPick res

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveHiddenResults ->
  ExprGuiM m (Maybe (ShownResult m), WidgetT m)
makeResultsWidget holeInfo shownResultsLists hiddenResults = do
  (mainResults, rows, mResults) <- unzip3 <$> traverse (makeResultGroup holeInfo) shownResultsLists
  let
    mSelectedResult = mResults ^? Lens.traversed . Lens._Just
    mFirstResult = mainResults ^? Lens.traversed
    mResult = mSelectedResult <|> mFirstResult
  addMResultPicker mResult
  hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
  widget <-
    if null rows
    then makeNoResults holeInfo (Widget.toAnimId myId)
    else
      return .
      Box.vboxCentered $
      ( OpenEventMap.blockDownEvents .
        Grid.toWidget . Grid.make
      . (map . map) ((,) (Vector2 0 0.5))
      ) rows :
      hiddenResultsWidgets
  return (mResult, widget)
  where
    myId = HoleInfo.hiActiveId holeInfo

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
      | otherwise = HoleInfo.hiActiveId holeInfo
    destId
      | null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
      | otherwise = head (shownMainResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

make ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload -> HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
make pl holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll config holeInfo
  let
    shownMainResultsIds = rId . (^. HoleResults.rlMain) <$> shownResultsLists
    allShownResultIds = [rId . (^. HoleResults.rlMain), (^. HoleResults.rlExtraResultsPrefixId)] <*> shownResultsLists
  assignHoleEditCursor
    holeInfo shownMainResultsIds allShownResultIds (hiSearchTermId holeInfo) $ do
      (mShownResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResultsLists hasHiddenResults
      (searchTermEventMap, resultsEventMap) <-
        OpenEventMap.make pl holeInfo mShownResult
      searchTermGui <-
        makeSearchTermGui holeInfo
        <&> ExpressionGui.egWidget %~
            Widget.weakerEvents searchTermEventMap
      let
        layers = Config.layers config
        layerDiff = Config.layerHoleBG layers - Config.layerMax layers
      searchTermGui
        & ExpressionGui.addBelow 0.5
          [(0.5, Widget.strongerEvents resultsEventMap resultsWidget)]
        & ExpressionGui.egWidget %~
          makeBackground (HoleInfo.hiActiveId holeInfo)
            (Config.layerMax (Config.layers config))
            (Config.activeHoleBGColor config)
        & ExpressionGui.addInferredTypes pl
        <&> ExpressionGui.egWidget . Widget.wFrame %~ Anim.onDepth (+ layerDiff)

makeSearchTermGui ::
  MonadA m => HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermGui holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv $
    (ExpressionGui.scaleFromTop (realToFrac <$> Config.holeSearchTermScaleFactor config) .
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
