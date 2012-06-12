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
import Editor.CTransaction (CTransaction, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CTransaction as CT
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

type ResultPicker m = Transaction ViewTag m Widget.EventResult

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

pasteEventMap :: MonadF m => Sugar.Hole m -> Widget.EventHandlers (Transaction ViewTag m)
pasteEventMap =
  maybe mempty
  (Widget.actionEventMapMovesCursor
   Config.pasteKeys "Paste" .
   liftM WidgetIds.fromGuid) .
  Sugar.holePaste

resultPickEventMap
  :: Result m -> Widget.EventHandlers (Transaction ViewTag m)
resultPickEventMap =
  maybe mempty (E.fromEventTypes Config.pickResultKeys "Pick this search result") .
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
  MonadF m => HoleInfo m -> Data.VariableRef -> CTransaction ViewTag m [Result m]
makeResultVariables holeInfo varRef = do
  varName <- CT.getP $ Anchors.variableNameRef varRef
  let
    parened =
      result
      (concat ["(", varName, ")"]) (return ()) resultIdAsPrefix .
      Parens.addTextParens $ Widget.toAnimId resultId
  return $
    case (Infix.isInfixName varName,
          Sugar.holeMFlipFuncArg (hiHole holeInfo)) of
    (True, Just flipAct) ->
      [result varName flipAct resultId return, parened]
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
     (Data.ExpressionI -> Transaction ViewTag m () -> ResultPicker m)
mPickResult holeInfo =
  fmap picker . Sugar.holePickResult $ hiHole holeInfo
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
  -> CTransaction ViewTag m [Result m]
makeAllResults holeInfo = do
  globals <- CT.getP Anchors.globals
  varResults <- liftM concat .
    mapM (makeResultVariables holeInfo) $
    params ++ globals
  searchTerm <- CT.getP $ hiSearchTerm holeInfo
  let
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
      paramTypeI <- DataOps.newHole
      resultTypeI <- DataOps.newHole
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
      mconcat $ pickFirstResultEventMaps ++
      case mPickResult holeInfo of
      Nothing -> []
      Just holePickResult ->
        [ E.fromEventTypes Config.newDefinitionKeys
          "Add new as Definition" $ makeNewDefinition holePickResult
        ]

    makeNewDefinition holePickResult = do
      searchTerm <- Property.get $ hiSearchTerm holeInfo
      let newName = concat . words $ searchTerm
      newDefI <- Anchors.makeDefinition newName -- TODO: From Sugar
      Anchors.newPane newDefI
      let defRef = Data.ExpressionGetVariable $ Data.DefinitionRef newDefI
      -- TODO: Can we use pickResult's animIdMapping?
      eventResult <- holePickResult defRef $ return ()
      maybe (return ()) Anchors.savePreJumpPosition $ Widget.eCursor eventResult
      return Widget.EventResult {
        Widget.eCursor = Just $ WidgetIds.fromIRef newDefI,
        Widget.eAnimIdMapping = holeResultAnimMappingNoParens holeInfo searchTermId
        }

makeResultsWidget
  :: MonadF m
  => [Result m] -> [Result m] -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (Result m), Widget (Transaction ViewTag m))
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
      Widget.actionEventMap
      [E.KeyEventType E.noMods E.KeyDown]
      "Nothing (at bottom)" (return ())

makeActiveHoleEdit
  :: MonadF m
  => HoleInfo m
  -> CTransaction ViewTag m
     (Maybe (Result m), Widget (Transaction ViewTag m))
makeActiveHoleEdit holeInfo =
  CT.assignCursor (hiHoleId holeInfo) searchTermId $ do
    CT.markVariablesAsUsed . Sugar.holeScope $ hiHole holeInfo

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
  -> CTransaction ViewTag m
     (Maybe (ResultPicker m), Widget (Transaction ViewTag m))
makeH hole guid myId = do
  cursor <- CT.readCursor
  searchText <- CT.getP $ hiSearchTerm holeInfo
  let
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
    holeInfo = HoleInfo
      { hiHoleId = myId
      , hiSearchTerm = Anchors.aDataRef "searchTerm" "" guid
      , hiHole = hole
      }
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground =
      Widget.backgroundColor 11 $
      mappend (Widget.toAnimId myId) ["hole background"]

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.KeyEventType E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Enter hole"
  , FocusDelegator.stopDelegatingKey = E.KeyEventType E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Leave hole"
  }

make
  :: MonadF m
  => Sugar.Hole m
  -> Guid
  -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (ResultPicker m), Widget (Transaction ViewTag m))
make hole =
  (fmap . liftM . second . Widget.weakerEvents) (pasteEventMap hole) .
  BWidgets.wrapDelegated holeFDConfig FocusDelegator.Delegating
  second . makeH hole
