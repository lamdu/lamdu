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

makeResult
  :: MonadF m
  => String -> ETypes.ExpressionPtr m -> Widget.Id
  -> Data.Expression -> NeedFlip m -> Widget.Id
  -> TWidget ViewTag m
  -> CTransaction ViewTag m (Result m)
makeResult name expressionPtr myId newExpr needFlip resultId
  mkWidget =
  do
    expressionI <- getP expressionPtr
    let pick = pickResult expressionI myId newExpr needFlip resultId
    return $ Result name pick mkWidget

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
  MonadF m => ETypes.ExpressionAncestry m ->
  Widget.Id -> ETypes.ExpressionPtr m ->
  Data.VariableRef ->
  CTransaction ViewTag m [Result m]
makeResultVariables ancestry myId expressionPtr varRef = do
  varName <- getP $ Anchors.variableNameRef varRef
  let
    ordinary = result varName DontFlip resultId return
    parened =
      result
      (concat ["(", varName, ")"]) DontFlip resultIdAsPrefix $
      ETypes.addParens id id myId
  sequence $
    if ETypes.isInfixName varName
    then
      case ancestry of
        (AncestryItemApply x@(ApplyParent ApplyArg _ _ _) : _) ->
          [result varName (DoFlip x) resultId return, parened]
        (AncestryItemApply (ApplyParent ApplyFunc _ _ _) : _) ->
          [ordinary]
        _ -> [parened]
    else
      [ordinary]
  where
    resultId = searchResultsPrefix myId `mappend` ETypes.varId varRef
    resultIdAsPrefix = Widget.joinId resultId ["prefix"]

    result name needFlip wid addParens =
      makeResult name expressionPtr myId getVar needFlip resultId $
        addParens =<< VarEdit.makeView varRef wid

    getVar = Data.ExpressionGetVariable varRef

getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

searchResultsPrefix :: Widget.Id -> Widget.Id
searchResultsPrefix = flip Widget.joinId ["search results"]

holeResultAnimMapping :: Widget.Id -> Widget.Id -> Widget.Id -> AnimId -> AnimId
holeResultAnimMapping myId resultId expressionId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) (Widget.toAnimId expressionId) .
  renamePrefix myAnimId ("old hole" : myAnimId)
  where
    myAnimId = Widget.toAnimId myId

pickResult
  :: MonadF m => IRef Data.Expression -> Widget.Id
  -> Data.Expression -> NeedFlip m -> Widget.Id
  -> ResultPicker m
pickResult expressionI myId newExpr needFlip resultId = do
  Transaction.writeIRef expressionI newExpr
  flipAct needFlip
  return Widget.EventResult {
    Widget.eCursor = Just expressionId,
    Widget.eAnimIdMapping =
      holeResultAnimMapping myId resultId expressionId
    }
  where
    expressionId = WidgetIds.fromIRef expressionI
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
  :: MonadF m
  => String -> ETypes.ExpressionPtr m
  -> Widget.Id
  -> CTransaction ViewTag m [Result m]
makeLiteralResults searchTerm expressionPtr myId =
  sequence
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    literalIntId = Widget.joinId (searchResultsPrefix myId) ["literal int"]
    makeLiteralIntResult integer =
      makeResult (show integer) expressionPtr myId
        (Data.ExpressionLiteralInteger integer) DontFlip
        literalIntId $
        BWidgets.makeFocusableView literalIntId =<<
        LiteralEdit.makeIntView literalIntId integer

makeAllResults
  :: MonadF m
  => ETypes.ExpressionAncestry m
  -> String
  -> Property (Transaction ViewTag m) Data.Definition
  -> ETypes.ExpressionPtr m -> Widget.Id
  -> CTransaction ViewTag m [Result m]
makeAllResults ancestry searchTerm definitionRef expressionPtr myId = do
  params <- liftM getDefinitionParamRefs $ getP definitionRef
  globals <- getP Anchors.globals
  varResults <- liftM concat .
    mapM (makeResultVariables ancestry myId expressionPtr) $
    sort params ++ sort globals
  literalResults <-
    makeLiteralResults searchTerm expressionPtr myId
  let goodResult = (searchTerm `isInfixOf`) . resultName
  return .
    sortOn (resultOrdering searchTerm) $
    literalResults ++ filter goodResult varResults


makeSearchTermWidget
  :: MonadF m
  => Widget.Id -> String -> [Result m]
  -> IRef Data.Expression -> Widget.Id
  -> TWidget ViewTag m
makeSearchTermWidget
  searchTermId searchTerm firstResults expressionI myId
  =
  (liftM . Widget.strongerEvents) searchTermEventMap $
    BWidgets.makeWordEdit searchTermRef searchTermId
  where
    pickFirstResultEventMaps =
      map resultPickEventMap $ take 1 firstResults

    newName = concat . words $ searchTerm
    searchTermEventMap =
      mconcat $ pickFirstResultEventMaps ++
      [ E.fromEventTypes Config.newDefinitionKeys "Add new as Definition" $ do
          newDef <- DataOps.addAsDefinition newName expressionI
          return Widget.EventResult {
            Widget.eCursor = Just $ WidgetIds.fromIRef newDef,
            Widget.eAnimIdMapping = holeResultAnimMapping myId searchTermId (WidgetIds.fromIRef expressionI)
            }
      ]
    searchTermRef =
      Property {
        get = return searchTerm,
        set =
          Transaction.writeIRef expressionI .
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
  => ETypes.ExpressionAncestry m
  -> IRef Data.Definition -> Data.HoleState
  -> ETypes.ExpressionPtr m
  -> Widget.Id
  -> CTransaction ViewTag m
     (Maybe (Result m), Widget (Transaction ViewTag m))
makeActiveHoleEdit
  ancestry definitionI (Data.HoleState searchTerm)
  expressionPtr myId =
  do
    expressionI <- getP expressionPtr
    let searchTermId = WidgetIds.searchTermId myId
    assignCursor myId searchTermId $ do
      let definitionRef = Transaction.fromIRef definitionI
      allResults <-
        makeAllResults ancestry searchTerm definitionRef
        expressionPtr myId

      let (firstResults, moreResults) = splitAt 3 allResults

      searchTermWidget <-
        makeSearchTermWidget searchTermId searchTerm firstResults
        expressionI myId

      (mResult, resultsWidget) <-
        makeResultsWidget firstResults moreResults myId
      return
        ( mplus mResult (listToMaybe $ take 1 firstResults)
        , BWidgets.vbox [searchTermWidget, resultsWidget] )

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
  if isJust (Widget.subId myId cursor)
    then
      (liftM . first . fmap) resultPick .
      (liftM . second)
        (makeBackground Config.focusedHoleBackgroundColor) $
      makeActiveHoleEdit ancestry definitionI curState expressionPtr myId
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
