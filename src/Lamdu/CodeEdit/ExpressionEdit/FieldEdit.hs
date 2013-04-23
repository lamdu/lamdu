{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
module Lamdu.CodeEdit.ExpressionEdit.FieldEdit
  ( make
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Lens.Operators
import Control.Monad (mplus, void)
import Control.MonadA (MonadA)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as List
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleCommon as HoleCommon
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction

fieldFDConfig :: FocusDelegator.Config
fieldFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey =
    E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc =
    E.Doc ["Edit", "Record", "Field", "Rename"]
  , FocusDelegator.stopDelegatingKey =
    E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc =
    E.Doc ["Edit", "Record", "Field", "Done renaming"]
  }

makeTagNameWidget ::
  MonadA m =>
  ((ExprGuiM.NameSource, String) -> ExprGuiM m (Widget f)) ->
  Guid -> ExprGuiM m (Widget f)
makeTagNameWidget f fieldGuid = do
  name@(nameSrc, _) <- ExprGuiM.transaction $ ExprGuiM.getGuidName fieldGuid
  ExpressionGui.nameSrcTint nameSrc .
    Widget.tint Config.fieldTint .
    Widget.scale Config.fieldScale <$> f name

makeTagNameView :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (Widget (T m))
makeTagNameView fieldGuid myId =
  makeTagNameWidget makeView fieldGuid
  where
    makeView (_, name) = ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView name myId

makeTagNameEdit :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (Widget (T m))
makeTagNameEdit fieldGuid myId =
  makeTagNameWidget makeEdit fieldGuid
  where
    makeEdit name = ExpressionGui.makeNameEdit name fieldGuid myId

make :: MonadA m => Sugar.FieldTag m -> ExprGuiM m (ExpressionGui m)
make fieldTag =
  case fieldTag ^. Sugar.ftTag of
  Nothing -> makeFieldTagHole fieldTag myId
  Just fieldGuid ->
    ExpressionGui.fromValueWidget . Widget.weakerEvents eventMap <$>
    ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
    (makeTagNameEdit fieldGuid) myId
  where
    myId = WidgetIds.fromGuid $ fieldTag ^. Sugar.ftGuid
    eventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor (Config.delKeys ++ Config.replaceKeys)
       (E.Doc ["Edit", "Field", "Replace"]) . (myId <$) . ($ Nothing)) $
      fieldTag ^. Sugar.ftMSetTag

makeFieldTagHole :: MonadA m => Sugar.FieldTag m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldTagHole fieldTag =
  ExpressionGui.wrapDelegated HoleCommon.holeFDConfig FocusDelegator.Delegating $
  makeUnwrappedHole fieldTag

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiFieldTag :: Sugar.FieldTag m
  , hiSearchTermProp :: Property (T m) String
  , hiSetTag :: Guid -> T m ()
  }

makeUnwrappedHole ::
  MonadA m => Sugar.FieldTag m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrappedHole fieldTag myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  case (fieldTag ^. Sugar.ftMSetTag, Widget.subId myId cursor) of
    (Just setTag, Just _) -> do
      searchTermProp <-
        ExprGuiM.transaction $
        Transaction.assocDataRefDef "" "SearchTerm"
        (fieldTag ^. Sugar.ftGuid) ^. Transaction.mkProperty
      makeActiveHoleEdit HoleInfo
        { hiHoleId = myId
        , hiFieldTag = fieldTag
        , hiSearchTermProp = searchTermProp
        , hiSetTag = setTag . Just
        }
    (x, _) -> HoleCommon.makeInactive (isJust x) myId

resultsPrefixId :: HoleInfo m -> Widget.Id
resultsPrefixId = HoleCommon.resultsPrefixId . hiHoleId

data Result = MakeNewFieldTag String | SetFieldTag Guid

pickResultAndCleanUp :: MonadA m => Anchors.CodeProps m -> HoleInfo m -> Result -> T m ()
pickResultAndCleanUp cp holeInfo res = do
  Property.set (hiSearchTermProp holeInfo) ""
  hiSetTag holeInfo =<<
    case res of
    MakeNewFieldTag str -> DataOps.makeNewFieldTag cp str
    SetFieldTag guid -> return guid

resultId :: HoleInfo m -> Result -> Widget.Id
resultId holeInfo result =
  resultsPrefixId holeInfo `mappend` f result
  where
    f (MakeNewFieldTag str) = Widget.Id ["new field", UTF8.fromString str]
    f (SetFieldTag guid) = WidgetIds.fromGuid guid

existingFieldResults :: MonadA m => Anchors.CodeProps m -> String -> T m [Result]
existingFieldResults cp searchTerm =
  fmap (map snd . HoleCommon.holeMatches fst searchTerm) .
  mapM fieldPair =<< Transaction.getP (Anchors.fields cp)
  where
    fieldPair guid = do
      name <- snd <$> ExprGuiM.getGuidName guid
      return ([name], SetFieldTag guid)

makeAllResults :: MonadA m => Anchors.CodeProps m -> String -> T m [Result]
makeAllResults cp searchTerm =
  (newFieldResults ++) <$> existingFieldResults cp searchTerm
  where
    newFieldResults =
      case searchTerm of
      [] -> []
      _ -> [MakeNewFieldTag searchTerm]

makeResultWidget :: MonadA m => HoleInfo m -> Result -> ExprGuiM m (Widget (T m))
makeResultWidget holeInfo res =
  case res of
  (MakeNewFieldTag _) ->
    fmap (Widget.scale Config.makeNewFieldSizeFactor) .
    ExprGuiM.widgetEnv $
    BWidgets.makeFocusableTextView "Make new field" myId
  (SetFieldTag fieldGuid) ->
    makeTagNameView fieldGuid myId
  where
    myId = resultId holeInfo res

makeResultsWidget ::
  MonadA m => HoleInfo m -> [Result] -> Bool ->
  ExprGuiM m (Widget (T m))
makeResultsWidget holeInfo firstResults moreResults =
  Box.vboxCentered <$>
  ((++) <$> mapM (makeResultWidget holeInfo) firstResults <*> makeMoreResultsWidgets moreResults)
  where
    makeMoreResultsWidgets False = return []
    makeMoreResultsWidgets True =
      ExprGuiM.widgetEnv . fmap (: []) . BWidgets.makeLabel "..." .
      Widget.toAnimId $ hiHoleId holeInfo

mkEventMap :: MonadA m => HoleInfo m -> T m () -> Widget.EventHandlers (T m)
mkEventMap holeInfo =
  Widget.keysEventMapMovesCursor Config.pickResultKeys (E.Doc ["Edit", "Result", "Pick"]) .
  -- Jump back out towards the guid of the whole field:
  (WidgetIds.fromGuid (hiFieldTag holeInfo ^. Sugar.ftGuid) <$)

makeActiveHoleEdit :: MonadA m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  cp <- ExprGuiM.readCodeAnchors
  (firstResults, hasMoreResults) <-
    (Lens._2 %~ not . null) .
    splitAt Config.fieldHoleResultCount <$>
    (ExprGuiM.transaction . makeAllResults cp .
     Property.value . hiSearchTermProp) holeInfo
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ resultsPrefixId holeInfo
    mSelectedResult = List.find (sub . resultId holeInfo) firstResults
    assignSource
      | shouldBeOnResult && isNothing mSelectedResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head $ (resultId holeInfo <$> firstResults) ++ [searchTermId]
    mResult = mSelectedResult `mplus` listToMaybe firstResults
  ExprGuiM.assignCursor assignSource destId $ do
    resultsWidget <- makeResultsWidget holeInfo firstResults hasMoreResults
    searchTermWidget <-
      HoleCommon.makeSearchTermWidget (hiSearchTermProp holeInfo) searchTermId
    let
      adHocEditor = HoleCommon.adHocTextEditEventMap $ hiSearchTermProp holeInfo
      pick = pickResultAndCleanUp cp holeInfo
      eventMap = maybe mempty (mkEventMap holeInfo . pick) mResult
    maybe (return ()) (ExprGuiM.addResultPicker . void . pick) mResult
    return .
      (ExpressionGui.egWidget %~)
      (Widget.strongerEvents eventMap .
       HoleCommon.makeBackground (hiHoleId holeInfo)
       Layers.activeHoleBG Config.holeBackgroundColor) $
      ExpressionGui.addBelow
      [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
      ]
      searchTermWidget
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo
