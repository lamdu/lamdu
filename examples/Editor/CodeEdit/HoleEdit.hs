{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.HoleEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.List(isInfixOf)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget, readCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.CodeEdit.VarEdit as VarEdit
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

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

makeResultVariables ::
  MonadF m => ETypes.ExpressionAncestry m ->
  Widget.Id -> ETypes.ExpressionPtr m ->
  Data.VariableRef ->
  CTransaction ViewTag m [Result m]
makeResultVariables ancestry searchResultsId expressionPtr varRef = do
  varName <- getP $ Anchors.variableNameRef varRef
  sequence $
    if ETypes.isOperatorName varName
    then
      case ancestry of
        ETypes.Argument argData ->
          [result varName (doFlip argData) myId dontAddParens,
           result
             (concat ["(", varName, ")"]) dontFlip myIdAsPrefix addParens]
        _ -> [result varName dontFlip myId dontAddParens]
    else
      [result varName dontFlip myId dontAddParens]
  where
    myId = searchResultsId `mappend` ETypes.varId varRef
    myIdAsPrefix = Widget.joinId myId ["prefix"]

    result name flipAct wid maybeAddParens = do
      pickEventMap <-
        pickResultEventMap ancestry expressionPtr
        searchResultsId getVar flipAct (ETypes.varId varRef)
      return $
        Result name pickEventMap $
          liftM (Widget.strongerEvents pickEventMap) .
          maybeAddParens $ VarEdit.makeView varRef wid

    getVar = Data.ExpressionGetVariable varRef

    dontAddParens = id
    addParens = (>>= ETypes.addParens myId)

    dontFlip = return ()
    doFlip (ETypes.ArgumentData _ parentPtr apply) = do
      parentI <- Property.get parentPtr
      Property.set (Transaction.fromIRef parentI) .
        Data.ExpressionApply $ flipArgs apply


getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

mapAnimId ::
  Widget.Id -> Widget.Id -> Widget.Id -> Anim.AnimId -> Anim.AnimId
mapAnimId expressionId searchResultsId varId animId =
  maybe animId mapSearchResult $
  Anim.subId (Widget.cursorId searchResultsId) animId
  where
    expressionAnimId = Widget.cursorId expressionId
    -- TODO: Is there a better way?
    getVariableTextAnimId = expressionAnimId
    mapOtherResult resultId =
      ["mismatched result"]
        `Anim.joinId` expressionAnimId
        `Anim.joinId` resultId
    mapSearchResult resultId =
      maybe (mapOtherResult resultId)
        (Anim.joinId getVariableTextAnimId) $
      (Anim.subId . Widget.cursorId) varId resultId

pickResultEventMap ::
  MonadF m => ETypes.ExpressionAncestry m ->
  ETypes.ExpressionPtr m -> Widget.Id ->
  Data.Expression -> Transaction ViewTag m () -> Widget.Id ->
  CTransaction ViewTag m (Widget.EventHandlers (Transaction ViewTag m))
pickResultEventMap
  ancestry expressionPtr searchResultsId expr flipAct varId =
  do
    expressionI <- getP expressionPtr
    return $ mconcat [
      EventMap.fromEventTypes Config.pickResultKeys
      "Pick this search result" $
      pickResult searchResultsId expressionI expr flipAct varId,

      EventMap.fromEventTypes Config.addNextArgumentKeys
      "Pick this search result and add argument" $
      pickResultAndAddArg
        ancestry expressionPtr searchResultsId expr flipAct varId
      ]

pickResultAndAddArg ::
  MonadF m => ETypes.ExpressionAncestry m ->
  ETypes.ExpressionPtr m -> Widget.Id ->
  Data.Expression -> Transaction ViewTag m () -> Widget.Id ->
  Transaction ViewTag m Widget.EventResult
pickResultAndAddArg
  ancestry expressionPtr searchResultsId expr flipAct varId =
  do
    expressionI <- Property.get expressionPtr
    res <- pickResult searchResultsId expressionI expr flipAct varId
    cursor <-
      ETypes.diveIn $
      case ancestry of
        ETypes.Argument (ETypes.ArgumentData { ETypes.adParentPtr = parentPtr }) ->
          DataOps.callWithArg parentPtr
        _ -> DataOps.callWithArg expressionPtr
    return res { Widget.eCursor = Just cursor }

pickResult ::
  MonadF m =>
  Widget.Id -> IRef Data.Expression ->
  Data.Expression -> Transaction ViewTag m () -> Widget.Id ->
  Transaction ViewTag m Widget.EventResult
pickResult searchResultsId expressionI expr flipAct varId =
  do
    Transaction.writeIRef expressionI expr
    flipAct
    return Widget.EventResult {
      Widget.eCursor = Just expressionId,
      Widget.eAnimIdMapping =
        mapAnimId expressionId searchResultsId varId
      }
    where
      expressionId = WidgetIds.fromIRef expressionI

flipArgs :: Data.Apply -> Data.Apply
flipArgs (Data.Apply x y) = Data.Apply y x

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
      let
        definitionRef = Transaction.fromIRef definitionI
        searchResultsId = Widget.joinId myId ["search results"]
      params <- liftM getDefinitionParamRefs $ getP definitionRef
      globals <- getP Anchors.globals
      allResults <-
        liftM concat .
        mapM
        (makeResultVariables ancestry searchResultsId expressionPtr) $
        params ++ globals

      let
        goodResult = (searchTerm `isInfixOf`) . resultName
        filteredResults = filter goodResult allResults

        (firstResults, moreResults) = splitAt 3 filteredResults

      let
        pickFirstResultEventMaps =
          map resultPickEventMap $ take 1 firstResults

        newName = concat . words $ searchTerm
        searchTermEventMap =
          mconcat $ pickFirstResultEventMaps ++
          [ Widget.actionEventMapMovesCursor Config.addParamKeys "Add as Parameter" .
            liftM WidgetIds.fromIRef $ DataOps.addAsParameter newName definitionRef expressionI
          , Widget.actionEventMapMovesCursor Config.newDefinitionKeys "Add new as Definition" $
            liftM WidgetIds.fromIRef $ DataOps.addAsDefinition newName expressionI
          ]
        stateProp =
          Property.Property {
            Property.get = return searchTerm,
            Property.set =
              Transaction.writeIRef expressionI .
              Data.ExpressionHole . Data.HoleState
            }

      searchTermWidget <-
        (liftM . Widget.strongerEvents) searchTermEventMap $
        BWidgets.makeWordEdit stateProp searchTermId

      resultWidgets <-
        case firstResults of
          [] -> makeNoResults myId
          _ ->
            liftM BWidgets.vbox $ mapM resultMakeWidget firstResults

      let
        mkMoreResultWidget
          | null moreResults = return []
          | otherwise = liftM (:[]) $ makeMoreResults myId

      moreResultsWidget <- mkMoreResultWidget
      return . BWidgets.vbox $ [searchTermWidget, resultWidgets] ++ moreResultsWidget

makeInternal ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  Widget.Id -> TWidget ViewTag m
makeInternal ancestry definitionI curState expressionPtr myId = do
  cursor <- readCursor
  widget <-
    if isJust (Widget.subId myId cursor)
    then makeActiveHoleEdit ancestry definitionI curState expressionPtr myId
    else BWidgets.makeFocusableTextView snippet $ WidgetIds.searchTermId myId
  return $ Widget.backgroundColor (Widget.joinId myId ["hole background"]) holeBackgroundColor widget
  where
    holeBackgroundColor = Draw.Color 0.8 0 0 0.3
    snippet
      | null searchText = "-"
      | otherwise = searchText
    searchText = Data.holeSearchTerm curState

make ::
  MonadF m =>
  ETypes.ExpressionAncestry m -> IRef Data.Definition ->
  Data.HoleState -> ETypes.ExpressionPtr m ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make ancestry definitionI holeState expressionPtr = do
  expressionId <- liftM WidgetIds.fromIRef $ getP expressionPtr
  BWidgets.wrapDelegatedWithKeys
    FocusDelegator.defaultKeys FocusDelegator.NotDelegating first
    ((fmap . liftM) (flip (,) expressionId) $
    makeInternal ancestry definitionI holeState expressionPtr) expressionId
