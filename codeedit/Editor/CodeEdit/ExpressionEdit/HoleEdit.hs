{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, ResultPicker) where

import Control.Arrow (first, second)
import Control.Monad (liftM, mplus)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.List.Utils (sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget, readCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

type ResultPicker m = Transaction ViewTag m Widget.EventResult

data Result m = Result {
  resultName :: String,
  resultPick :: ResultPicker m,
  resultMakeWidget :: TWidget ViewTag m
  }

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiHole :: Sugar.Hole m
  }

resultPickEventMap
  :: Result m -> Widget.EventHandlers (Transaction ViewTag m)
resultPickEventMap =
  E.fromEventTypes Config.pickResultKeys "Pick this search result" .
  resultPick

resultToWidget :: Monad m => Result m -> TWidget ViewTag m
resultToWidget result =
  (liftM . Widget.strongerEvents)
  (resultPickEventMap result) $
  resultMakeWidget result

makeNoResults :: MonadF m => Widget.Id -> TWidget t m
makeNoResults myId =
  BWidgets.makeTextView "(No results)" $
  Widget.joinId myId ["no results"]

makeMoreResults :: MonadF m => Widget.Id -> TWidget t m
makeMoreResults myId =
  BWidgets.makeTextView "..." $
  Widget.joinId myId ["more results"]

makeResultVariables ::
  MonadF m => HoleInfo m -> Data.VariableRef -> CTransaction ViewTag m [Result m]
makeResultVariables holeInfo varRef = do
  varName <- getP $ Anchors.variableNameRef varRef
  let
    ordinary = result varName (return ()) resultId return
    parened =
      result
      (concat ["(", varName, ")"]) (return ()) resultIdAsPrefix $
      Parens.addTextParens resultId
  return $
    if Infix.isInfixName varName
    then
      case Sugar.holeMFlipFuncArg (hiHole holeInfo) of
      Nothing -> [ordinary]
      Just flipAct -> [result varName flipAct resultId return, parened]
    else
      [ordinary]
  where
    resultId = searchResultsPrefix (hiHoleId holeInfo) `mappend` WidgetIds.varId varRef
    resultIdAsPrefix = Widget.joinId resultId ["prefix"]
    result name flipAct wid addParens =
      Result
      { resultName = name
      , resultPick = pickResult holeInfo (Data.ExpressionGetVariable varRef) flipAct
      , resultMakeWidget = addParens =<< VarEdit.makeView varRef wid
      }

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

pickResult
  :: MonadF m
  => HoleInfo m
  -> Data.Expression -> Transaction ViewTag m ()
  -> ResultPicker m
pickResult _holeInfo newExpr flipAct = do
  Transaction.writeIRef (error "TODO") newExpr
  flipAct
  return Widget.EventResult {
    Widget.eCursor = Just $ error "TODO",
    Widget.eAnimIdMapping = id -- TODO: Need to fix the parens id
    }

resultOrdering :: String -> Result m -> (Bool, Bool)
resultOrdering searchTerm result =
  (searchTerm /= name, not (searchTerm `isPrefixOf` name))
  where
    name = resultName result

makeLiteralResults
  :: MonadF m => HoleInfo m -> String -> [Result m]
makeLiteralResults holeInfo searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    literalIntId = Widget.joinId (searchResultsPrefix (hiHoleId holeInfo)) ["literal int"]
    makeLiteralIntResult integer =
      Result
      { resultName = show integer
      , resultPick = pickResult holeInfo (Data.ExpressionLiteralInteger integer) (return ())
      , resultMakeWidget =
          BWidgets.makeFocusableView literalIntId =<<
          LiteralEdit.makeIntView literalIntId integer
      }

makeAllResults
  :: MonadF m
  => HoleInfo m -> String
  -> CTransaction ViewTag m [Result m]
makeAllResults holeInfo searchTerm = do
  globals <- getP Anchors.globals
  varResults <- liftM concat .
    mapM (makeResultVariables holeInfo) $
    params ++ sort globals
  let
    literalResults = makeLiteralResults holeInfo searchTerm
    goodResult = (searchTerm `isInfixOf`) . resultName
  return .
    sortOn (resultOrdering searchTerm) $
    literalResults ++ filter goodResult varResults
  where
    params = Sugar.holeScope $ hiHole holeInfo

makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id -> String -> [Result m] -> TWidget ViewTag m
makeSearchTermWidget holeInfo searchTermId searchTerm firstResults =
  (liftM . Widget.strongerEvents) searchTermEventMap $
    BWidgets.makeWordEdit searchTermRef searchTermId
  where
    pickFirstResultEventMaps =
      map resultPickEventMap $ take 1 firstResults

    -- newName = concat . words $ searchTerm
    searchTermEventMap =
      mconcat $ pickFirstResultEventMaps ++
      [ E.fromEventTypes Config.newDefinitionKeys "Add new as Definition" $ do
          newDef <- error "addAsDefinition TODO" -- DataOps.addAsDefinition newName $ hiExpressionI holeInfo
          Anchors.savePreJumpPosition $ hiHoleId holeInfo
          return Widget.EventResult {
            Widget.eCursor = Just $ WidgetIds.fromIRef newDef,
            Widget.eAnimIdMapping = holeResultAnimMappingNoParens holeInfo searchTermId
            }
      ]
    searchTermRef =
      Property {
        get = return searchTerm,
        set =
          Transaction.writeIRef (error "TODO setHoleState") .
          Data.ExpressionHole . Data.HoleState
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
      [] -> liftM ((,) Nothing) $ makeNoResults myId
      xs -> do
        let
          widget = blockDownEvents . BWidgets.vbox $ map snd xs
          mResult =
            listToMaybe . map fst $
            filter (Widget.isFocused . snd) xs
        return (mResult, widget)
  let
    makeMoreResultWidgets [] = return []
    makeMoreResultWidgets _ = liftM (: []) $ makeMoreResults myId
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
  -> Data.HoleState
  -> CTransaction ViewTag m
     (Maybe (Result m), Widget (Transaction ViewTag m))
makeActiveHoleEdit holeInfo (Data.HoleState searchTerm) =
  assignCursor (hiHoleId holeInfo) searchTermId $ do
    allResults <- makeAllResults holeInfo searchTerm

    let (firstResults, moreResults) = splitAt 3 allResults

    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId searchTerm firstResults

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
  -> Sugar.ExpressionRef m
  -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (ResultPicker m), Widget (Transaction ViewTag m))
makeH hole _expressionRef myId = do
  cursor <- readCursor
  let
    holeInfo = HoleInfo
      { hiHoleId = myId
      , hiHole = hole
      }
  if isJust (Widget.subId myId cursor)
    then
      liftM (
        first (fmap resultPick) .
        second (makeBackground Config.focusedHoleBackgroundColor)) $
      makeActiveHoleEdit holeInfo (Sugar.holeState hole)
    else
      liftM
      ((,) Nothing .
       makeBackground Config.unfocusedHoleBackgroundColor) .
      BWidgets.makeFocusableTextView snippet $
      WidgetIds.searchTermId myId
  where
    makeBackground =
      Widget.backgroundColor $ Widget.joinId myId ["hole background"]
    snippet
      | null searchText = "  "
      | otherwise = searchText
    searchText = Data.holeSearchTerm $ Sugar.holeState hole

make
  :: MonadF m
  => Sugar.Hole m
  -> Sugar.ExpressionRef m
  -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (ResultPicker m), Widget (Transaction ViewTag m))
make hole =
  BWidgets.wrapDelegatedWithKeys FocusDelegator.defaultKeys FocusDelegator.Delegating second .
  makeH hole
