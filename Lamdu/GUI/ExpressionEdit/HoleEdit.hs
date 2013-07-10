{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  , eventResultOfPickedResult
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>), liftA2)
import Control.Lens.Operators
import Control.Monad (guard, msum, when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Maybe (isJust, maybeToList, fromMaybe)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Data.Store.Property (Property(..))
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CharClassification (operatorChars, alphaNumericChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleState(..), hsSearchTerm, diveIntoHole)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Results (ResultsList(..), Result(..), HaveHiddenResults(..))
import Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.AddNextHoles as AddNextHoles
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data ShownResult m = ShownResult
  { srEventMap :: Widget.EventHandlers (T m)
  , srHoleResult :: Sugar.HoleResult Sugar.Name m HoleResults.SugarExprPl
  }

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

eventResultOfPickedResult :: Sugar.PickedResult -> Widget.EventResult
eventResultOfPickedResult pr =
  Widget.EventResult
  { Widget._eCursor = Monoid.Last $ WidgetIds.fromGuid <$> pr ^. Sugar.prMJumpTo
  , Widget._eAnimIdMapping =
    Monoid.Endo $ pickedResultAnimIdTranslation (pr ^. Sugar.prIdTranslation)
  }
  where
    pickedResultAnimIdTranslation idTranslations =
      -- Map only the first anim id component
      Lens.ix 0 %~ \x -> fromMaybe x $ Map.lookup x idMap
      where
        idMap =
          idTranslations
          & Lens.traversed . Lens.both %~ head . Widget.toAnimId . WidgetIds.fromGuid
          & Map.fromList

afterPick :: Monad m => HoleInfo m -> Sugar.PickedResult -> T m Widget.EventResult
afterPick holeInfo pr = do
  Property.set (hiState holeInfo) HoleInfo.emptyState
  eventResultOfPickedResult pr
    & Widget.eCursor %~
      (mappend . Monoid.Last . Just .
       WidgetIds.fromGuid . hiStoredGuid) holeInfo
    & return

setNextHoleState ::
  MonadA m =>
  HoleInfo m -> String -> Sugar.PickedResult -> T m Widget.EventResult
setNextHoleState holeInfo searchTerm pr =
  afterPick holeInfo pr <*
  case pr ^. Sugar.prMJumpTo of
    Just newHoleGuid ->
      Transaction.setP (HoleInfo.assocStateRef newHoleGuid) $ HoleState searchTerm
    Nothing -> return ()

resultPickEventMap ::
  MonadA m => Config -> HoleInfo m -> Maybe (ShownResult m) ->
  Widget.EventHandlers (T m)
resultPickEventMap _ _ Nothing = mempty
resultPickEventMap config holeInfo (Just shownResult) =
  mappend alphaNumericAfterOperator $
  -- TODO: Does this guid business make sense?
  case hiHoleGuids holeInfo ^. ExprGuiM.hgMNextHole of
  Just nextHoleGuid
    | not (srHoleResult shownResult ^. Sugar.holeResultHasHoles) ->
      mappend (simplePickRes (Config.pickResultKeys config)) .
      E.keyPresses (Config.pickAndMoveToNextHoleKeys config)
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) $
        (Widget.eCursor .~
         (Monoid.Last . Just . WidgetIds.fromGuid) nextHoleGuid) <$>
        pick
  _ ->
    simplePickRes $
    Config.pickResultKeys config ++
    Config.pickAndMoveToNextHoleKeys config
  where
    searchTerm = HoleInfo.hiSearchTerm holeInfo
    alphaNumericAfterOperator
      | nonEmptyAll (`elem` operatorChars) searchTerm =
        E.charGroup "Letter/digit"
        (E.Doc ["Edit", "Result", "Pick and resume"])
        alphaNumericChars $ \c _ -> setNextHoleState holeInfo [c] =<< holeResultPick
      | otherwise = mempty
    holeResultPick = srHoleResult shownResult ^. Sugar.holeResultPick
    pick = afterPick holeInfo =<< holeResultPick
    simplePickRes keys =
      E.keyPresses keys (E.Doc ["Edit", "Result", "Pick"]) pick

makePaddedResult :: MonadA m => Result m -> ExprGuiM m (WidgetT m)
makePaddedResult res = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  mkWidget (rId res) (rHoleResult res)
    <&> (Widget.pad . fmap realToFrac . Config.holeResultPadding) config
  where
    mkWidget =
      case rInfo res of
      HoleResults.ResultInfoNewTag -> makeNewTagResultWidget
      HoleResults.ResultInfoNormal -> makeHoleResultWidget

makeShownResult ::
  MonadA m => Result m -> ExprGuiM m (Widget (T m), ShownResult m)
makeShownResult result = do
  widget <- makePaddedResult result
  return
    ( widget & Widget.wEventMap .~ mempty
    , ShownResult
      { srEventMap = widget ^. Widget.wEventMap
      , srHoleResult = rHoleResult result
      }
    )

makeResultGroup ::
  MonadA m =>
  ResultsList m ->
  ExprGuiM m
  ( ShownResult m
  , [WidgetT m]
  , Maybe (ShownResult m)
  )
makeResultGroup results = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (mainResultWidget, shownMainResult) <- makeShownResult mainResult
  extraSymbolWidget <-
    if Lens.has (HoleResults.rlExtra . traverse) results
    then
      BWidgets.hboxCenteredSpaced . (Spacer.empty :) . (: []) .
      Widget.scale extraSymbolScaleFactor <$>
      ExprGuiM.widgetEnv
      (BWidgets.makeLabel extraSymbol (Widget.toAnimId (rId mainResult)))
    else pure Spacer.empty
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
    makeExtra = makeExtraResultsWidget $ results ^. HoleResults.rlExtra

makeExtraResultsPlaceholderWidget ::
  MonadA m => [Result m] -> ExprGuiM m (WidgetT m)
makeExtraResultsPlaceholderWidget [] = return Spacer.empty
makeExtraResultsPlaceholderWidget (result:_) =
  makeFocusable (rId result) Spacer.empty

makeExtraResultsWidget ::
  MonadA m => [Result m] ->
  ExprGuiM m (Maybe (ShownResult m), WidgetT m)
makeExtraResultsWidget [] = return (Nothing, Spacer.empty)
makeExtraResultsWidget extraResults@(firstResult:_) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    mkResWidget result = do
      isOnResult <- ExprGuiM.widgetEnv $ WE.isSubCursor (rId result)
      (widget, shownResult) <- makeShownResult result
      return
        ( shownResult <$ guard isOnResult
        , widget
        )
  (mResults, widgets) <-
    unzip <$> traverse mkResWidget extraResults
  return
    ( msum mResults
    , Box.vboxAlign 0 widgets
      & makeBackground (rId firstResult)
        (Config.layerMax (Config.layers config))
        (Config.activeHoleBackgroundColor config)
      & Widget.wSize .~ (head widgets ^. Widget.wSize & Lens._1 .~ 0)
    )

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId

makeHoleResultWidget ::
  MonadA m => Widget.Id ->
  Sugar.HoleResult Sugar.Name m HoleResults.SugarExprPl -> ExprGuiM m (WidgetT m)
makeHoleResultWidget resultId holeResult = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  resultGui <-
    ExprGuiM.makeSubexpression 0 .
    SugarRemoveTypes.holeResultTypes .
    postProcessSugar $ holeResult ^. Sugar.holeResultConverted
  resultGui ^. ExpressionGui.egWidget
    & Widget.wFrame %~ Anim.mapIdentities (`mappend` Widget.toAnimId resultId)
    & Widget.scale (realToFrac <$> Config.holeResultScaleFactor config)
    & makeFocusable resultId

postProcessSugar ::
  MonadA m =>
  Sugar.ExpressionN m HoleResults.SugarExprPl ->
  Sugar.ExpressionN m ExprGuiM.Payload
postProcessSugar expr =
  expr
  & Lens.mapped . Lens.mapped %~ toPayload
  -- TODO: AddNextHoles on hole result is unused?
  & AddNextHoles.addToExpr
  -- Remove the top-level result's actions so that they come from our
  -- ExpressionEdit, rather than the result's ExpressionEdit which
  -- represents the same IRef
  & Sugar.rPayload . Sugar.plData . ExprGuiM.plHoleGuids .~ ExprGuiM.emptyHoleGuids
  & Sugar.rPayload . Sugar.plActions .~ Nothing

toPayload :: HoleResults.SugarExprPl -> ExprGuiM.Payload
toPayload (ExprGuiM.StoredGuids guids, ExprGuiM.Injected injected) =
  ExprGuiM.Payload
  { ExprGuiM._plStoredGuids = guids
  , ExprGuiM._plInjected = injected
  -- filled by AddNextHoles above
  , ExprGuiM._plHoleGuids = ExprGuiM.emptyHoleGuids
  }

asNewLabelScaleFactor :: Fractional a => a
asNewLabelScaleFactor = 0.5

makeNewTagResultWidget ::
  MonadA m =>
  Widget.Id -> Sugar.HoleResult Sugar.Name m HoleResults.SugarExprPl ->
  ExprGuiM m (WidgetT m)
makeNewTagResultWidget resultId holeResult = do
  widget <- makeHoleResultWidget resultId holeResult
  ExprGuiM.widgetEnv $ do
    label <-
      fmap (Widget.scale asNewLabelScaleFactor) .
      BWidgets.makeLabel " (as new tag)" $ Widget.toAnimId resultId
    return $ Box.hboxAlign 0.5 [widget, label]

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

blockDownEvents :: Monad f => Widget f -> Widget f
blockDownEvents =
  Widget.weakerEvents $
  E.keyPresses
  [E.ModKey E.noMods E.KeyDown]
  (E.Doc ["Navigation", "Move", "down (blocked)"]) $
  return mempty

addSelectedResultPicker :: MonadA m => Maybe (ShownResult m) -> ExprGuiM m ()
addSelectedResultPicker mSelectedResult =
  case mSelectedResult of
    Nothing -> return ()
    Just res -> ExprGuiM.addResultPicker . void $ srHoleResult res ^. Sugar.holeResultPick

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveHiddenResults ->
  ExprGuiM m (Maybe (ShownResult m), WidgetT m)
makeResultsWidget holeInfo shownResultsLists hiddenResults = do
  (mainResults, rows, mResults) <- unzip3 <$> traverse makeResultGroup shownResultsLists
  let
    mSelectedResult = mResults ^? Lens.traversed . Lens._Just
    mFirstResult = mainResults ^? Lens.traversed
    mResult = mSelectedResult <|> mFirstResult
  addSelectedResultPicker mSelectedResult
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
    myId = HoleInfo.hiActiveId holeInfo

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
      | otherwise = HoleInfo.hiActiveId holeInfo
    destId
      | null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
      | otherwise = head (shownResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

makeActive ::
  MonadA m =>
  Widget.Size ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload -> HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeActive inactiveSize pl holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll config holeInfo
  let
    shownResultsIds = rId . (^. HoleResults.rlMain) <$> shownResultsLists
    allResultIds = [rId . (^. HoleResults.rlMain), (^. HoleResults.rlExtraResultsPrefixId)] <*> shownResultsLists
  assignHoleEditCursor
    holeInfo shownResultsIds allResultIds (hiSearchTermId holeInfo) $ do
      (mShownResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResultsLists hasHiddenResults
      searchTermWidget <- makeSearchTermWidget holeInfo
      let
        adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
        layers = Config.layers config
        layerDiff = Config.layerActiveHoleBG layers - Config.layerMax layers
      jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap [] pl
      replaceEventMap <- ExprEventMap.replaceOrComeToParentEventMap pl
      let
        eventMap = mconcat
          [ pasteEventMap config holeInfo
          , resultEventMap config holeInfo mShownResult
          , jumpHolesEventMap
          , replaceEventMap
          , closeEventMap
          ]
      ExpressionGui.addBelow 0.5
        [(0.5, Widget.strongerEvents adHocEditor resultsWidget)]
        searchTermWidget
        & ExpressionGui.egWidget %~
          (Widget.wFrame %~ Anim.onDepth (+ layerDiff)) .
          makeBackground (HoleInfo.hiActiveId holeInfo)
            (Config.layerMax (Config.layers config))
            (Config.activeHoleBackgroundColor config) .
          Widget.weakerEvents eventMap .
          Widget.strongerEvents
          (resultPickEventMap config holeInfo mShownResult)
        & ExpressionGui.addInferredTypes pl
        <&> resize
  where
    resize gui =
      ExpressionGui.truncateSize
      ( inactiveSize
        & Lens._1 %~ max (gui ^. ExpressionGui.egWidget . Widget.wSize . Lens._1)
      ) gui
    closeEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.KeyEsc]
      (E.Doc ["Navigation", "Hole", "Close"]) . pure $
      Widget.joinId (hiId holeInfo) ["inactive"]

pasteEventMap ::
  Functor m => Config -> HoleInfo m -> Widget.EventHandlers (T m)
pasteEventMap config holeInfo =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) $ hiActions holeInfo ^. Sugar.holePaste

resultEventMap ::
  MonadA m => Config -> HoleInfo m -> Maybe (ShownResult m) ->
  Widget.EventHandlers (T m)
resultEventMap _ _ Nothing = mempty
resultEventMap config holeInfo (Just (ShownResult eventMap holeResult)) =
  eventMap
  & maybe id (mappend . extraResultEventMap) mActions
  & Lens.mapped %~
    liftA2 mappend
    (afterPick holeInfo =<<
     holeResult ^. Sugar.holeResultPick)
  where
    extraResultEventMap = mconcat
      [ ExprEventMap.applyOperatorEventMap []
      , ExprEventMap.cutEventMap config
      ]
    convertedResult = holeResult ^. Sugar.holeResultConverted
    mActions = convertedResult ^. Sugar.rPayload . Sugar.plActions

tryActiveHole ::
  MonadA m =>
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Size -> Widget.Id ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryActiveHole hole pl inactiveSize myId = do
  isSelected <-
    lift . ExprGuiM.widgetEnv . WE.isSubCursor $ diveIntoHole myId
  guard isSelected
  storedGuid <- maybeToMPlus $ pl ^? Sugar.plActions . Lens._Just . Sugar.storedGuid
  actions <- maybeToMPlus $ hole ^. Sugar.holeMActions
  inferred <- maybeToMPlus $ hole ^. Sugar.holeMInferred
  stateProp <- lift . ExprGuiM.transaction $ HoleInfo.assocStateRef storedGuid ^. Transaction.mkProperty
  lift $ makeActive inactiveSize pl HoleInfo
    { hiStoredGuid = storedGuid
    , hiActions = actions
    , hiInferred = inferred
    , hiId = myId
    , hiState = stateProp
    , hiHoleGuids = pl ^. Sugar.plData . ExprGuiM.plHoleGuids
    , hiMArgument = hole ^. Sugar.holeMArg
    }

makeInactiveWrapper ::
  MonadA m =>
  Sugar.HoleArg m (Sugar.ExpressionN m ExprGuiM.Payload) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactiveWrapper arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    bgColor =
      case arg ^. Sugar.haUnwrap of
      Sugar.UnwrapMAction {} -> Config.deletableHoleBackgroundColor config
      Sugar.UnwrapTypeMismatch {} -> Config.typeErrorHoleWrapBackgroundColor config
    eventMap =
      case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
      Just unwrap ->
        E.keyPresses (Config.acceptKeys config ++ Config.delKeys config)
        (E.Doc ["Edit", "Unwrap"]) $
        Widget.eventResultFromCursor . WidgetIds.fromGuid <$> unwrap
      Nothing ->
        E.keyPresses (Config.wrapKeys config)
        (E.Doc ["Navigation", "Hole", "Open"]) .
        pure . Widget.eventResultFromCursor $
        diveIntoHole myId
  arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId . (Widget.wEventMap .~ eventMap)
    <&> ExpressionGui.pad (realToFrac <$> Config.wrapperHolePadding config)
    <&> ExpressionGui.egWidget %~
        makeBackground myId
        (Config.layerInactiveHole (Config.layers config)) bgColor

makeInactiveInferred ::
  MonadA m =>
  Sugar.HoleInferred m -> Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (Widget.Id, ExpressionGui m)
makeInactiveInferred inferred pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  gui <-
    iVal
    & Lens.mapped .~ emptyPl
    & InputExpr.makePure gen
    & Sugar.runConvert (pl ^. Sugar.plConvertInContext)
      (Sugar.hiContext inferred)
    & ExprGuiM.liftMemoT
    <&> Lens.mapped . Lens.mapped %~ toPayload
    >>= ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId .
        Widget.tint (Config.inferredValueTint config) .
        Widget.scale (realToFrac <$> Config.inferredValueScaleFactor config) .
        (Widget.wEventMap .~ mempty)
  return $
    if fullyInferred
    then (myId, gui)
    else
      ( diveIntoHole myId
      , gui
        & ExpressionGui.egWidget %~
          makeBackground myId (Config.layerInactiveHole (Config.layers config))
          (Config.inactiveHoleBackgroundColor config)
      )
  where
    fullyInferred = Lens.nullOf (Lens.folding ExprUtil.subExpressions . ExprLens.exprHole) iVal
    iVal = Infer.iValue $ Sugar.hiInferred inferred
    -- TODO: should gen still be compatible with the anim id
    -- translations of PickedResult? If so, document it here
    gen = genFromHashable $ pl ^. Sugar.plGuid
    emptyPl = (ExprGuiM.StoredGuids [], ExprGuiM.Injected [])

makeInactiveSimple :: MonadA m => Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactiveSimple myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv
    (BWidgets.makeTextViewWidget "  " (Widget.toAnimId myId))
    <&>
      makeBackground myId
      (Config.layerInactiveHole (Config.layers config))
      (Config.inactiveHoleBackgroundColor config)
    <&> ExpressionGui.fromValueWidget
    >>= ExpressionGui.egWidget %%~ makeFocusable myId

createInactive ::
  MonadA m =>
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (Widget.Id, ExpressionGui m)
createInactive hole pl myId = do
  (destId, rawInactive) <- runMatcherT $ do
    justToLeft $ do
      arg <- maybeToMPlus $ hole ^. Sugar.holeMArg
      lift $ (,) myId <$> makeInactiveWrapper arg myId
    justToLeft $ do
      inferred <- maybeToMPlus $ hole ^. Sugar.holeMInferred
      guard . Lens.nullOf ExprLens.exprHole . Infer.iValue $ Sugar.hiInferred inferred
      lift $ makeInactiveInferred inferred pl myId
    lift $ (,) (diveIntoHole myId) <$> makeInactiveSimple myId
  exprEventMap <- ExprEventMap.make [] pl
  inactive <-
    ExpressionGui.addInferredTypes pl rawInactive
    <&> ExpressionGui.egWidget %~
        Widget.weakerEvents (mappend openEventMap exprEventMap)
  return (destId, inactive)
  where
    openEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.KeyEnter]
      (E.Doc ["Navigation", "Hole", "Open"]) . pure $
      diveIntoHole myId

make ::
  MonadA m =>
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole pl myId = do
  (delegateDestId, inactive) <- createInactive hole pl myId
  ExprGuiM.assignCursor myId delegateDestId $ do
    let inactiveSize = inactive ^. ExpressionGui.egWidget . Widget.wSize
    fromMaybe inactive <$>
      runMaybeT (tryActiveHole hole pl inactiveSize myId)

-- TODO: Use this where the hiState is currently used to get the
-- search term
searchTermProperty :: HoleInfo m -> Property (T m) String
searchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap searchTermProp =
  mconcat . concat $
  [ [ disallowChars (Property.value searchTermProp) .
      E.simpleChars "Character"
      (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace])
      (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) searchTermProp
    ]
  ]
  where
    changeText f = mempty <$ Property.pureModify searchTermProp f

disallowChars :: String -> E.EventMap a -> E.EventMap a
disallowChars searchTerm =
  E.filterSChars (curry (`notElem` disallowedHoleChars)) .
  E.deleteKey (keyPress E.KeySpace) .
  E.deleteKey (keyPress E.KeyEnter) .
  disallowMix
  where
    disallowMix
      | nonEmptyAll (`notElem` operatorChars) searchTerm =
        E.filterSChars (curry (`notElem` E.anyShiftedChars operatorChars))
      | nonEmptyAll (`elem` operatorChars) searchTerm =
        E.filterSChars (curry (`notElem` E.anyShiftedChars alphaNumericChars))
      | otherwise = id
    keyPress = E.KeyEvent E.Press . E.ModKey E.noMods

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

makeSearchTermWidget ::
  MonadA m => HoleInfo m ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget holeInfo = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv $
    (ExpressionGui.scaleFromTop (realToFrac <$> Config.holeSearchTermScaleFactor config) .
     ExpressionGui.fromValueWidget .
     (Widget.wEventMap %~ disallowChars searchTerm) .
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
        then Widget.eCursor .~ (Monoid.Last . Just . HoleResults.prefixId) holeInfo
        else id

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]
