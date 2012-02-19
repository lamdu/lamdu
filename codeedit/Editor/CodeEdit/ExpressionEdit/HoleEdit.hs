{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make) where

import Control.Monad (liftM)
import Data.List(isInfixOf, isPrefixOf, sort, sortBy)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Ord(comparing)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, transaction, getP, assignCursor, TWidget, readCursor)
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
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget

data Result m = Result {
  resultName :: String,
  resultPickEventMap :: Widget.EventHandlers (Transaction ViewTag m),
  resultMakeWidget :: TWidget ViewTag m
  }

makeNoResults :: MonadF m => Widget.Id -> TWidget t m
makeNoResults myId =
  BWidgets.makeTextView "(No results)" $
  Widget.joinId myId ["no results"]

makeMoreResults :: MonadF m => Widget.Id -> TWidget t m
makeMoreResults myId =
  BWidgets.makeTextView "..." $
  Widget.joinId myId ["more results"]

data NeedFlip m = DontFlip | DoFlip (ETypes.ApplyData m)

makeResultVariables ::
  MonadF m => ETypes.ExpressionAncestry m ->
  Widget.Id -> ETypes.ExpressionPtr m ->
  Data.VariableRef ->
  CTransaction ViewTag m [Result m]
makeResultVariables ancestry myId expressionPtr varRef = do
  varName <- getP $ Anchors.variableNameRef varRef
  sequence $
    if ETypes.isInfixName varName
    then
      case ancestry of
        (x @ ETypes.ApplyData { ETypes.adRole = ETypes.ApplyArg } : xs) ->
          [result varName (DoFlip x) resultId
           ((ETypes.atAdRole . const) ETypes.ApplyFunc x : xs)
          ,result
           (concat ["(", varName, ")"]) DontFlip resultIdAsPrefix
           ancestry
          ]
        _ -> [result varName DontFlip resultId ancestry]
    else
      [result varName DontFlip resultId ancestry]
  where
    resultId = searchResultsPrefix myId `mappend` ETypes.varId varRef
    resultIdAsPrefix = Widget.joinId resultId ["prefix"]

    result name needFlip wid resultAncestry = do
      pickEventMap <-
        pickResultEventMap ancestry expressionPtr myId getVar needFlip resultId
      return .
        Result name pickEventMap .
        liftM (Widget.strongerEvents pickEventMap) $
        VarEdit.makeView resultAncestry varRef wid

    getVar = Data.ExpressionGetVariable varRef

getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

pickResultEventMap
  :: MonadF m => ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m -> Widget.Id -> Data.Expression
  -> NeedFlip m -> Widget.Id
  -> CTransaction ViewTag m (Widget.EventHandlers (Transaction ViewTag m))
pickResultEventMap
  ancestry expressionPtr myId expr needFlip resultId =
  do
    expressionI <- getP expressionPtr
    (addArgDoc, addArgHandler) <-
      transaction $ ETypes.makeAddArgHandler ancestry expressionPtr
    let
      pickResultAndAddArg = do
        res <- pickResult myId expressionI expr needFlip resultId
        cursor <- addArgHandler
        return res { Widget.eCursor = Just cursor }
    return $ mconcat [
      EventMap.fromEventTypes Config.pickResultKeys
      "Pick this search result" $
      pickResult myId expressionI expr needFlip resultId,

      EventMap.fromEventTypes Config.addNextArgumentKeys
      ("Pick this search result and " ++ addArgDoc)
      pickResultAndAddArg
      ]

searchResultsPrefix :: Widget.Id -> Widget.Id
searchResultsPrefix = flip Widget.joinId ["search results"]

holeResultAnimMapping :: Widget.Id -> Widget.Id -> Widget.Id -> AnimId -> AnimId
holeResultAnimMapping myId resultId expressionId =
  renamePrefix ("old hole" : Widget.cursorId resultId) (Widget.cursorId expressionId) .
  renamePrefix myAnimId ("old hole" : myAnimId)
  where
    myAnimId = Widget.cursorId myId

pickResult
  :: MonadF m => Widget.Id -> IRef Data.Expression
  -> Data.Expression -> NeedFlip m -> Widget.Id
  -> Transaction ViewTag m Widget.EventResult
pickResult myId expressionI expr needFlip resultId = do
  Transaction.writeIRef expressionI expr
  flipAct needFlip
  return Widget.EventResult {
    Widget.eCursor = Just expressionId,
    Widget.eAnimIdMapping =
      holeResultAnimMapping myId resultId expressionId
    }
  where
    flipAct DontFlip = return ()
    flipAct (DoFlip (ETypes.ApplyData _ _ apply parentPtr)) = do
      parentI <- Property.get parentPtr
      Property.set (Transaction.fromIRef parentI) .
        Data.ExpressionApply $ flipArgs apply

    expressionId = WidgetIds.fromIRef expressionI

flipArgs :: Data.Apply -> Data.Apply
flipArgs (Data.Apply x y) = Data.Apply y x

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

resultOrdering :: String -> Result m -> (Bool, Bool)
resultOrdering searchTerm result =
  (searchTerm /= name, not (searchTerm `isPrefixOf` name))
  where
    name = resultName result

makeLiteralResults
  :: MonadF m
  => ETypes.ExpressionAncestry m
  -> String -> ETypes.ExpressionPtr m
  -> Widget.Id
  -> CTransaction ViewTag m [Result m]
makeLiteralResults ancestry searchTerm expressionPtr myId =
  sequence
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    literalIntId = Widget.joinId (searchResultsPrefix myId) ["literal int"]
    makeLiteralIntResult integer = do
      pickEventMap <-
        pickResultEventMap ancestry expressionPtr
        myId (Data.ExpressionLiteralInteger integer) DontFlip literalIntId
      return .
        Result (show integer) pickEventMap .
        liftM (Widget.strongerEvents pickEventMap) .
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
    makeLiteralResults ancestry searchTerm expressionPtr myId
  let goodResult = (searchTerm `isInfixOf`) . resultName
  return .
    sortOn (resultOrdering searchTerm) $
    literalResults ++ filter goodResult varResults


makeSearchTermWidget
  :: MonadF m
  => Widget.Id -> String -> [Result m]
  -> Transaction.Property ViewTag m Data.Definition
  -> IRef Data.Expression -> Widget.Id
  -> TWidget ViewTag m
makeSearchTermWidget
  searchTermId searchTerm firstResults definitionRef expressionI myId
  =
  (liftM . Widget.strongerEvents) searchTermEventMap $
    BWidgets.makeWordEdit searchTermRef searchTermId
  where
    pickFirstResultEventMaps =
      map resultPickEventMap $ take 1 firstResults

    newName = concat . words $ searchTerm
    searchTermEventMap =
      mconcat $ pickFirstResultEventMaps ++
      [ EventMap.fromEventTypes Config.addAsParamKeys "Add as Parameter" $ do
          DataOps.addAsParameter newName definitionRef expressionI
          let exprId = WidgetIds.fromIRef expressionI
          return Widget.EventResult {
            Widget.eCursor = Just exprId,
            Widget.eAnimIdMapping = holeResultAnimMapping myId searchTermId exprId
            }
      , EventMap.fromEventTypes Config.newDefinitionKeys "Add new as Definition" $ do
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
  -> TWidget ViewTag m
makeResultsWidget firstResults moreResults myId = do
  firstResultsWidgets <- mapM resultMakeWidget firstResults
  firstResultsWidget <-
    case firstResultsWidgets of
      [] -> makeNoResults myId
      xs -> return $ BWidgets.vbox xs
  let
    makeMoreResultWidgets [] = return []
    makeMoreResultWidgets _ = liftM (: []) $ makeMoreResults myId
  moreResultsWidgets <- makeMoreResultWidgets moreResults

  return $ BWidgets.vbox (firstResultsWidget : moreResultsWidgets)

blockEvents :: MonadF m => Widget m -> Widget m
blockEvents = Widget.weakerEvents blockUpDownEventMap
  where
    blockUpDownEventMap = mconcat [
      blockEvent EventMap.KeyUp "top",
      blockEvent EventMap.KeyDown "bottom"]
    blockEvent key side =
      Widget.actionEventMap [EventMap.KeyEventType EventMap.noMods key]
      ("Nothing (at " ++ side ++ ")") (return ())

makeActiveHoleEdit ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState -> ETypes.ExpressionPtr m ->
  Widget.Id -> TWidget ViewTag m
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
        definitionRef expressionI myId

      resultsWidget <- makeResultsWidget firstResults moreResults myId
      return . blockEvents $
        BWidgets.vbox [searchTermWidget, resultsWidget]

make ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  Widget.Id -> TWidget ViewTag m
make ancestry definitionI curState expressionPtr myId = do
  cursor <- readCursor
  widget <-
    if isJust (Widget.subId myId cursor)
    then
      makeBackground Config.focusedHoleBackgroundColor $
      makeActiveHoleEdit ancestry definitionI curState expressionPtr myId
    else
      makeBackground Config.unfocusedHoleBackgroundColor .
      BWidgets.makeFocusableTextView snippet $ WidgetIds.searchTermId myId
  return widget
  where
    makeBackground = liftM . (Widget.backgroundColor (Widget.joinId myId ["hole background"]))
    snippet
      | null searchText = "  "
      | otherwise = searchText
    searchText = Data.holeSearchTerm curState
