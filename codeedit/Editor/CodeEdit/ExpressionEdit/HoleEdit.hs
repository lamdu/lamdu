{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, ResultPicker) where

import Control.Arrow (first, second)
import Control.Monad (liftM, mplus)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.List.Utils (sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget, readCursor)
import Editor.CodeEdit.Types(AncestryItem(..), ApplyParent(..), ApplyRole(..))
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

type ResultPicker m = Transaction ViewTag m Widget.EventResult

data Result m = Result {
  resultName :: String,
  resultPick :: ResultPicker m,
  resultMakeWidget :: TWidget ViewTag m
  }

data HoleInfo m = HoleInfo
  { hiExpressionI :: IRef Data.Expression
  , hiHoleId :: Widget.Id
  , hiAncestry :: ETypes.ExpressionAncestry m
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

data NeedFlip m = DontFlip | DoFlip (ApplyParent m)

makeResultVariables ::
  MonadF m => HoleInfo m -> Data.VariableRef -> CTransaction ViewTag m [Result m]
makeResultVariables holeInfo varRef = do
  varName <- getP $ Anchors.variableNameRef varRef
  let
    ordinary = result varName DontFlip resultId return
    parened =
      result
      (concat ["(", varName, ")"]) DontFlip resultIdAsPrefix $
      ETypes.addParens id id resultId
  return $
    if ETypes.isInfixName varName
    then
      case hiAncestry holeInfo of
        (AncestryItemApply x@(ApplyParent ApplyArg _ _ _) : _) ->
          [result varName (DoFlip x) resultId return, parened]
        (AncestryItemApply (ApplyParent ApplyFunc _ _ _) : _) ->
          [ordinary]
        _ -> [parened]
    else
      [ordinary]
  where
    resultId = searchResultsPrefix (hiHoleId holeInfo) `mappend` ETypes.varId varRef
    resultIdAsPrefix = Widget.joinId resultId ["prefix"]
    result name needFlip wid addParens =
      Result
      { resultName = name
      , resultPick = pickResult holeInfo (Data.ExpressionGetVariable varRef) needFlip resultId
      , resultMakeWidget = addParens =<< VarEdit.makeView varRef wid
      }

getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

searchResultsPrefix :: Widget.Id -> Widget.Id
searchResultsPrefix = flip Widget.joinId ["search results"]

holeResultAnimMappingNoParens :: HoleInfo m -> Widget.Id -> AnimId -> AnimId
holeResultAnimMappingNoParens holeInfo resultId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) (Widget.toAnimId expressionId) .
  renamePrefix myId ("old hole" : myId)
  where
    myId = Widget.toAnimId $ hiHoleId holeInfo
    expressionId = WidgetIds.fromIRef $ hiExpressionI holeInfo

holeResultAnimMapping :: HoleInfo m -> Widget.Id -> Widget.Id -> AnimId -> AnimId
holeResultAnimMapping holeInfo resultId parensId =
  renamePrefix (Widget.toAnimId (ETypes.parensPrefix expressionId)) (Widget.toAnimId (ETypes.parensPrefix parensId)) .
  holeResultAnimMappingNoParens holeInfo resultId
  where
    expressionId = WidgetIds.fromIRef $ hiExpressionI holeInfo

pickResult
  :: MonadF m => HoleInfo m
  -> Data.Expression -> NeedFlip m -> Widget.Id
  -> ResultPicker m
pickResult holeInfo newExpr needFlip resultId = do
  Transaction.writeIRef (hiExpressionI holeInfo) newExpr
  flipAct needFlip
  parensId <- ETypes.makeParensId $ hiAncestry holeInfo
  return Widget.EventResult {
    Widget.eCursor = Just . WidgetIds.fromIRef $ hiExpressionI holeInfo,
    Widget.eAnimIdMapping =
      holeResultAnimMapping holeInfo resultId parensId
    }
  where
    flipAct DontFlip = return ()
    flipAct (DoFlip (ApplyParent _ _ apply parentPtr)) = do
      parentI <- Property.get parentPtr
      Property.set (Transaction.fromIRef parentI) .
        Data.ExpressionApply $ flipArgs apply

flipArgs :: Data.Apply -> Data.Apply
flipArgs (Data.Apply x y) = Data.Apply y x

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
      , resultPick = pickResult holeInfo (Data.ExpressionLiteralInteger integer) DontFlip literalIntId
      , resultMakeWidget =
          BWidgets.makeFocusableView literalIntId =<<
          LiteralEdit.makeIntView literalIntId integer
      }

makeAllResults
  :: MonadF m
  => HoleInfo m -> String
  -> Property (Transaction ViewTag m) Data.Definition
  -> CTransaction ViewTag m [Result m]
makeAllResults holeInfo searchTerm definitionRef = do
  defParams <- liftM getDefinitionParamRefs $ getP definitionRef
  let
    allLambdaParams = ETypes.getAncestryParams $ hiAncestry holeInfo
    params = defParams ++ map Data.ParameterRef allLambdaParams
  globals <- getP Anchors.globals
  varResults <- liftM concat .
    mapM (makeResultVariables holeInfo) $
    sort params ++ sort globals
  let
    literalResults = makeLiteralResults holeInfo searchTerm
    goodResult = (searchTerm `isInfixOf`) . resultName
  return .
    sortOn (resultOrdering searchTerm) $
    literalResults ++ filter goodResult varResults

makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id -> String -> [Result m] -> TWidget ViewTag m
makeSearchTermWidget holeInfo searchTermId searchTerm firstResults =
  (liftM . Widget.strongerEvents) searchTermEventMap $
    BWidgets.makeWordEdit searchTermRef searchTermId
  where
    pickFirstResultEventMaps =
      map resultPickEventMap $ take 1 firstResults

    newName = concat . words $ searchTerm
    searchTermEventMap =
      mconcat $ pickFirstResultEventMaps ++
      [ E.fromEventTypes Config.newDefinitionKeys "Add new as Definition" $ do
          newDef <- DataOps.addAsDefinition newName $ hiExpressionI holeInfo
          return Widget.EventResult {
            Widget.eCursor = Just $ WidgetIds.fromIRef newDef,
            Widget.eAnimIdMapping = holeResultAnimMappingNoParens holeInfo searchTermId
            }
      ]
    searchTermRef =
      Property {
        get = return searchTerm,
        set =
          Transaction.writeIRef (hiExpressionI holeInfo) .
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
  -> IRef Data.Definition -> Data.HoleState
  -> CTransaction ViewTag m
     (Maybe (Result m), Widget (Transaction ViewTag m))
makeActiveHoleEdit holeInfo definitionI (Data.HoleState searchTerm) =
  assignCursor (hiHoleId holeInfo) searchTermId $ do
    let definitionRef = Transaction.fromIRef definitionI
    allResults <- makeAllResults holeInfo searchTerm definitionRef

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

make
  :: MonadF m
  => ETypes.ExpressionAncestry m
  -> IRef Data.Definition
  -> Data.HoleState
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (ResultPicker m), Widget (Transaction ViewTag m))
make ancestry definitionI curState expressionPtr myId = do
  cursor <- readCursor
  expressionI <- getP expressionPtr
  let
    holeInfo = HoleInfo
      { hiExpressionI = expressionI
      , hiHoleId = myId
      , hiAncestry = ancestry
      }
  if isJust (Widget.subId myId cursor)
    then
      liftM (
        first (fmap resultPick) .
        second (makeBackground Config.focusedHoleBackgroundColor)) $
      makeActiveHoleEdit holeInfo definitionI curState
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
    searchText = Data.holeSearchTerm curState
