{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, ResultPicker) where

import Control.Arrow (first, second)
import Control.Monad (liftM, mplus)
import Data.ByteString (ByteString)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget, WidgetT)
import Graphics.UI.Bottle.Animation(AnimId)
import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

type ResultPicker m = ITransaction ViewTag m Widget.EventResult

data Result m = Result {
  resultName :: String,
  resultPick :: Maybe (ResultPicker m),
  resultMakeWidget :: TWidget ViewTag m
  }

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiSearchTerm :: Property (Transaction ViewTag m) String
  , hiHole :: Sugar.Hole m
  }

pasteEventMap :: MonadF m => Sugar.Hole m -> Widget.EventHandlers (ITransaction ViewTag m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys "Paste" .
   liftM WidgetIds.fromGuid .
   IT.transaction) .
  Sugar.holePaste

resultPickEventMap
  :: Result m -> Widget.EventHandlers (ITransaction ViewTag m)
resultPickEventMap =
  maybe mempty (E.keyPresses Config.pickResultKeys "Pick this search result") .
  resultPick

resultToWidget :: Monad m => Result m -> TWidget ViewTag m
resultToWidget result =
  (liftM . Widget.strongerEvents)
  (resultPickEventMap result) $
  resultMakeWidget result

makeNoResults :: MonadF m => AnimId -> TWidget t m
makeNoResults myId =
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

makeMoreResults :: MonadF m => AnimId -> TWidget t m
makeMoreResults myId =
  BWidgets.makeTextView "..." $ mappend myId ["more results"]

makeResultVariables ::
  MonadF m => HoleInfo m -> Data.VariableRef -> OTransaction ViewTag m [Result m]
makeResultVariables holeInfo varRef = do
  varName <- OT.getP $ Anchors.variableNameRef varRef
  let
    parened =
      result
      (concat ["(", varName, ")"]) (return ()) resultIdAsPrefix .
      Parens.addTextParens $ Widget.toAnimId resultId
  return $
    case (Infix.isInfixName varName,
          Sugar.holeMFlipFuncArg (hiHole holeInfo)) of
    (True, Just flipAct) ->
      [result varName (IT.transaction flipAct) resultId return, parened]
    _ ->
      [result varName (return ()) resultId return]
  where
    resultId =
      searchResultsPrefix (hiHoleId holeInfo) `mappend`
      WidgetIds.varId varRef
    resultIdAsPrefix = Widget.joinId resultId ["prefix"]
    result name flipAct wid addParens =
      Result
      { resultName = name
      , resultPick = fmap (pickGetVariable flipAct) $ mPickResult holeInfo
      , resultMakeWidget = addParens =<< VarEdit.makeView varRef wid
      }
    pickGetVariable flipAct pickResult =
      pickResult (Data.ExpressionGetVariable varRef) flipAct

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

searchResultsPrefix :: Widget.Id -> Widget.Id
searchResultsPrefix = flip Widget.joinId ["search results"]

holeResultAnimMappingNoParens :: HoleInfo m -> Widget.Id -> AnimId -> AnimId
holeResultAnimMappingNoParens holeInfo resultId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) myId .
  renamePrefix myId ("old hole" : myId)
  where
    myId = Widget.toAnimId $ hiHoleId holeInfo

mPickResult
  :: MonadF m
  => HoleInfo m
  -> Maybe
     (Data.ExpressionI -> ITransaction ViewTag m () -> ResultPicker m)
mPickResult holeInfo =
  fmap (picker . fmap IT.transaction) . Sugar.holePickResult $ hiHole holeInfo
  where
    picker holePickResult newExpr flipAct = do
      guid <- holePickResult newExpr
      ~() <- flipAct
      return Widget.EventResult {
        Widget.eCursor = Just $ WidgetIds.fromGuid guid,
        Widget.eAnimIdMapping = id -- TODO: Need to fix the parens id
        }

resultOrdering :: String -> Result m -> [Bool]
resultOrdering searchTerm result =
  map not
  [ searchTerm == name
  , searchTerm `isPrefixOf` name
  , Function.on isPrefixOf (map Char.toLower) searchTerm name
  , searchTerm `isInfixOf` name
  ]
  where
    name = resultName result

searchResultId :: HoleInfo m -> ByteString -> Widget.Id
searchResultId holeInfo = Widget.joinId (searchResultsPrefix (hiHoleId holeInfo)) . (:[])

makeLiteralResults
  :: MonadF m => HoleInfo m -> String -> [Result m]
makeLiteralResults holeInfo searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    literalIntId = searchResultId holeInfo "literal int"
    makeLiteralIntResult integer =
      Result
      { resultName = show integer
      , resultPick = fmap (pickLiteralInt integer) $ mPickResult holeInfo
      , resultMakeWidget =
          BWidgets.makeFocusableView literalIntId =<<
          LiteralEdit.makeIntView (Widget.toAnimId literalIntId) integer
      }
    pickLiteralInt integer holePickResult = holePickResult (Data.ExpressionLiteralInteger integer) (return ())

makeAllResults
  :: MonadF m
  => HoleInfo m
  -> OTransaction ViewTag m [Result m]
makeAllResults holeInfo = do
  globals <- OT.getP Anchors.globals
  varResults <- liftM concat .
    mapM (makeResultVariables holeInfo) $
    params ++ globals
  let
    searchTerm = Property.value $ hiSearchTerm holeInfo
    literalResults = makeLiteralResults holeInfo searchTerm
    goodResult = Function.on isInfixOf (map Char.toLower) searchTerm . resultName
  return .
    sortOn (resultOrdering searchTerm) $
    literalResults ++ filter goodResult (piResult : varResults)
  where
    params = Sugar.holeScope $ hiHole holeInfo
    piResult =
      Result
      { resultName = "->"
      , resultPick = fmap pickPiResult $ mPickResult holeInfo
      , resultMakeWidget =
          BWidgets.makeFocusableTextView "→" $ searchResultId holeInfo "Pi result"
      }
    pickPiResult holePickResult = do
      paramTypeI <- IT.transaction DataOps.newHole
      resultTypeI <- IT.transaction DataOps.newHole
      holePickResult (Data.ExpressionPi (Data.Lambda paramTypeI resultTypeI)) (return ())

makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id -> [Result m] -> TWidget ViewTag m
makeSearchTermWidget holeInfo searchTermId firstResults =
  liftM
    (Widget.strongerEvents searchTermEventMap .
     (Widget.atEventMap . E.filterChars) (`notElem` "[]\\")) $
    BWidgets.makeWordEdit (hiSearchTerm holeInfo) searchTermId
  where
    pickFirstResultEventMaps =
      map resultPickEventMap $ take 1 firstResults

    searchTermEventMap =
      mconcat pickFirstResultEventMaps `mappend`
      maybe mempty
      (E.keyPresses Config.newDefinitionKeys
       "Add new as Definition" . makeNewDefinition)
      (mPickResult holeInfo)

    makeNewDefinition holePickResult = do
      newDefI <- IT.transaction $ do
        newDefI <- Anchors.makeDefinition -- TODO: From Sugar
        let
          searchTerm = Property.value $ hiSearchTerm holeInfo
          newName = concat . words $ searchTerm
        Anchors.setP (Anchors.assocNameRef (IRef.guid newDefI)) newName
        Anchors.newPane newDefI
        return newDefI
      let defRef = Data.ExpressionGetVariable $ Data.DefinitionRef newDefI
      -- TODO: Can we use pickResult's animIdMapping?
      eventResult <- holePickResult defRef $ return ()
      maybe (return ()) (IT.transaction . Anchors.savePreJumpPosition) $ Widget.eCursor eventResult
      return Widget.EventResult {
        Widget.eCursor = Just $ WidgetIds.fromIRef newDefI,
        Widget.eAnimIdMapping = holeResultAnimMappingNoParens holeInfo searchTermId
        }

makeResultsWidget
  :: MonadF m
  => [Result m] -> [Result m] -> Widget.Id
  -> OTransaction ViewTag m
     (Maybe (Result m), WidgetT ViewTag m)
makeResultsWidget firstResults moreResults myId = do
  let
    resultAndWidget result =
      liftM ((,) result) $ resultToWidget result
  firstResultsAndWidgets <- mapM resultAndWidget firstResults
  (mResult, firstResultsWidget) <-
    case firstResultsAndWidgets of
      [] -> liftM ((,) Nothing) . makeNoResults $ Widget.toAnimId myId
      xs -> do
        let
          widget = blockDownEvents . BWidgets.vbox $ map snd xs
          mResult =
            listToMaybe . map fst $
            filter (Widget.isFocused . snd) xs
        return (mResult, widget)
  let
    makeMoreResultWidgets [] = return []
    makeMoreResultWidgets _ = liftM (: []) $ makeMoreResults $ Widget.toAnimId myId
  moreResultsWidgets <- makeMoreResultWidgets moreResults

  return (mResult, BWidgets.vbox (firstResultsWidget : moreResultsWidgets))
  where
    blockDownEvents =
      Widget.weakerEvents $
      Widget.keysEventMap
      [E.ModKey E.noMods E.KeyDown]
      "Nothing (at bottom)" (return ())

makeActiveHoleEdit
  :: MonadF m
  => HoleInfo m
  -> OTransaction ViewTag m
     (Maybe (Result m), WidgetT ViewTag m)
makeActiveHoleEdit holeInfo =
  OT.assignCursor (hiHoleId holeInfo) searchTermId $ do
    OT.markVariablesAsUsed . Sugar.holeScope $ hiHole holeInfo

    allResults <- makeAllResults holeInfo

    let (firstResults, moreResults) = splitAt 3 allResults

    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId firstResults

    (mResult, resultsWidget) <-
      makeResultsWidget firstResults moreResults $ hiHoleId holeInfo
    return
      ( mplus mResult (listToMaybe $ take 1 firstResults)
      , BWidgets.vbox [searchTermWidget, resultsWidget] )
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

makeH
  :: MonadF m
  => Sugar.Hole m
  -> Guid
  -> Widget.Id
  -> OTransaction ViewTag m
     (Maybe (ResultPicker m), WidgetT ViewTag m)
makeH hole guid myId = do
  cursor <- OT.readCursor
  searchTermProp <- OT.transaction $ Anchors.assocDataRef "searchTerm" "" guid
  let
    holeInfo = HoleInfo
      { hiHoleId = myId
      , hiSearchTerm = searchTermProp
      , hiHole = hole
      }
    searchText = Property.value searchTermProp
    snippet
      | null searchText = "  "
      | otherwise = searchText
  if isJust (Widget.subId myId cursor) && canPickResult
    then
      liftM
      (first (>>= resultPick) .
       second (makeBackground Config.focusedHoleBackgroundColor)) $
      makeActiveHoleEdit holeInfo
    else
      liftM
      ((,) Nothing .
       makeBackground unfocusedColor) .
      BWidgets.makeFocusableTextView snippet $
      WidgetIds.searchTermId myId
  where
    unfocusedColor
      | canPickResult = Config.unfocusedHoleBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground =
      Widget.backgroundColor 11 $
      mappend (Widget.toAnimId myId) ["hole background"]

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Enter hole"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Leave hole"
  }

make
  :: MonadF m
  => Sugar.Hole m
  -> Guid
  -> Widget.Id
  -> OTransaction ViewTag m
     (Maybe (ResultPicker m), WidgetT ViewTag m)
make hole =
  (fmap . liftM . second . Widget.weakerEvents) (pasteEventMap hole) .
  BWidgets.wrapDelegated holeFDConfig FocusDelegator.Delegating
  second . makeH hole
