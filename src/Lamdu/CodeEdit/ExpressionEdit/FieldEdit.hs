{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
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

makeTagNameEdit :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (Widget (T m))
makeTagNameEdit fieldGuid myId = do
  name@(nameSrc, _) <- ExprGuiM.getGuidName fieldGuid
  ExpressionGui.nameSrcTint nameSrc <$>
    ExpressionGui.makeNameEdit name fieldGuid myId

make :: MonadA m => Sugar.FieldTag m -> ExprGuiM m (ExpressionGui m)
make fieldTag =
  case fieldTag ^. Sugar.ftTag of
  Nothing -> makeFieldTagHole fieldTag myId
  Just fieldGuid ->
    ExpressionGui.fromValueWidget <$>
    ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
    (makeTagNameEdit fieldGuid) myId
  where
    myId = WidgetIds.fromGuid $ fieldTag ^. Sugar.ftGuid

makeFieldTagHole :: MonadA m => Sugar.FieldTag m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldTagHole fieldTag =
  ExpressionGui.wrapDelegated HoleCommon.holeFDConfig FocusDelegator.Delegating $
  makeUnwrapped fieldTag

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiFieldTag :: Sugar.FieldTag m
  , hiSearchTermProp :: Property (T m) String
  , hiSetTag :: Guid -> T m ()
  }

makeUnwrapped ::
  MonadA m => Sugar.FieldTag m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped fieldTag myId = do
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

pickResult :: MonadA m => Anchors.CodeProps m -> HoleInfo m -> Result -> T m ()
pickResult cp holeInfo res =
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

makeAllResults :: MonadA m => HoleInfo m -> ([Result], Bool)
makeAllResults holeInfo = (newFieldResults, False)
  where
    newFieldResults =
      case Property.value (hiSearchTermProp holeInfo) of
      [] -> []
      searchTerm -> [MakeNewFieldTag searchTerm]

makeResultWidget :: MonadA m => HoleInfo m -> Result -> ExprGuiM m (Widget (T m))
makeResultWidget holeInfo res =
  case res of
  (MakeNewFieldTag _) ->
    fmap (Widget.scale Config.makeNewFieldSizeFactor) .
    ExprGuiM.widgetEnv $
    BWidgets.makeFocusableTextView "Make new field" myId
  (SetFieldTag fieldGuid) ->
    makeTagNameEdit fieldGuid myId
  where
    myId = resultId holeInfo res

makeResultsWidget ::
  MonadA m => HoleInfo m -> [Result] -> Bool ->
  ExprGuiM m (Widget (T m))
makeResultsWidget holeInfo firstResults moreResults =
  Widget.scale Config.holeResultScaleFactor . Box.vboxCentered <$>
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
  let
    (firstResults, hasMoreResults) = makeAllResults holeInfo
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
    cp <- ExprGuiM.readCodeAnchors
    let
      adHocEditor = HoleCommon.adHocTextEditEventMap $ hiSearchTermProp holeInfo
      pick = pickResult cp holeInfo
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
