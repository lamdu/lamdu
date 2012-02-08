{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.HoleEdit(make) where

import Control.Monad (liftM, liftM2)
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

data ResultType m =
  InfixOperator (ETypes.ArgumentData m) |
  PrefixOperator |
  NotOperator

data Result m = Result {
  _resultType :: ResultType m,
  _resultVar :: Data.VariableRef
  }

getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

makeResultView ::
  (MonadF m) => Widget.Id -> Result m ->
  CTransaction t m (Widget (Transaction t m))
makeResultView myId (Result typ var) =
  maybeAddParens typ .
  VarEdit.makeView var . mappend myId .
  maybeAddPrefixWrap typ $
  ETypes.varId var
  where
    maybeAddPrefixWrap PrefixOperator = (`Widget.joinId` ["prefix"])
    maybeAddPrefixWrap _ = id
    maybeAddParens PrefixOperator = (>>= ETypes.addParens (ETypes.varId var))
    maybeAddParens _ = id

pickResultEventMap ::
  MonadF m => ETypes.ExpressionAncestry m ->
  ETypes.ExpressionPtr m -> Widget.Id -> Result m ->
  CTransaction ViewTag m (Widget.EventHandlers (Transaction ViewTag m))
pickResultEventMap ancestry expressionPtr searchResultsId var = do
  expressionI <- getP expressionPtr
  return $ mconcat [
    EventMap.fromEventTypes Config.pickResultKeys
    "Pick this search result" $
    pickResult searchResultsId expressionI var,

    EventMap.fromEventTypes Config.addNextArgumentKeys
    "Pick this search result and add argument" $
    pickResultAndAddArg ancestry expressionPtr searchResultsId var
    ]

pickResultAndAddArg ::
  MonadF m => ETypes.ExpressionAncestry m ->
  ETypes.ExpressionPtr m -> Widget.Id -> Result m ->
  Transaction ViewTag m Widget.EventResult
pickResultAndAddArg ancestry expressionPtr searchResultsId result = do
  expressionI <- Property.get expressionPtr
  res <- pickResult searchResultsId expressionI result
  cursor <-
    ETypes.diveIn $
    case ancestry of
      ETypes.Argument (ETypes.ArgumentData { ETypes.adParentPtr = parentPtr }) ->
        DataOps.callWithArg parentPtr
      _ -> DataOps.callWithArg expressionPtr
  return res { Widget.eCursor = Just cursor }

mapAnimId ::
  Widget.Id -> Widget.Id -> Data.VariableRef -> Anim.AnimId -> Anim.AnimId
mapAnimId expressionId searchResultsId var animId =
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
      (Anim.subId . Widget.cursorId . ETypes.varId) var resultId

pickResult ::
  MonadF m =>
  Widget.Id -> IRef Data.Expression -> Result m ->
  Transaction ViewTag m Widget.EventResult
pickResult searchResultsId expressionI (Result rType var) = do
  Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
  case rType of
    InfixOperator (ETypes.ArgumentData { ETypes.adParentPtr = parentPtr, ETypes.adApply = apply}) -> do
      parentI <- Property.get parentPtr
      Property.set (Transaction.fromIRef parentI) . Data.ExpressionApply $ flipArgs apply
    _ -> return ()
  return Widget.EventResult {
    Widget.eCursor = Just expressionId,
    Widget.eAnimIdMapping = mapAnimId expressionId searchResultsId var
    }
  where
    expressionId = WidgetIds.fromIRef expressionI

flipArgs :: Data.Apply -> Data.Apply
flipArgs (Data.Apply x y) = Data.Apply y x

makeActiveHoleEdit ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState -> ETypes.ExpressionPtr m ->
  Widget.Id -> TWidget ViewTag m
makeActiveHoleEdit ancestry definitionI curState expressionPtr myId = do
  expressionI <- getP expressionPtr
  let
    maybeResults [] = BWidgets.makeTextView "(No results)" $ Widget.joinId myId ["no results"]
    maybeResults xs = liftM BWidgets.vbox $ mapM makeResultWidget xs

    searchTermId = WidgetIds.searchTermId myId
    searchResultsId = Widget.joinId myId ["search results"]
    makeResultWidget result =
      liftM2 Widget.strongerEvents
        (pickResultEventMap ancestry expressionPtr searchResultsId result)
        (makeResultView searchResultsId result)

    stateProp =
      Property.Property {
        Property.get = return $ Data.holeSearchTerm curState,
        Property.set = Transaction.writeIRef expressionI . Data.ExpressionHole . (`Data.atHoleSearchTerm` curState) . const
      }
    goodResult (_, name) = all (`isInfixOf` name) . words $ Data.holeSearchTerm curState
    definitionRef = Transaction.fromIRef definitionI
    newName = concat . words $ Data.holeSearchTerm curState
    processResult (var, name)
      | ETypes.isOperatorName name =
        case ancestry of
          ETypes.Argument argData ->
            [Result (InfixOperator argData) var,
             Result PrefixOperator var]
          _ -> [Result PrefixOperator var]
      | otherwise = [Result NotOperator var]

  assignCursor myId searchTermId $ do
    params <- liftM getDefinitionParamRefs $ getP definitionRef
    globals <- getP Anchors.globals
    let addVarName var = liftM ((,) var) . getP $ Anchors.variableNameRef var
    vars <- mapM addVarName $ params ++ globals
    let
      results = concatMap processResult $ filter goodResult vars
      (firstResults, moreResults) = splitAt 3 results
      mkMoreResultWidget
        | null moreResults = return []
        | otherwise = liftM (:[]) . BWidgets.makeTextView "..." $ Widget.joinId myId ["more results"]

    pickFirstResultEventMaps <- mapM (pickResultEventMap ancestry expressionPtr searchResultsId) $ take 1 firstResults
    let
      searchTermEventMap =
        mconcat $ pickFirstResultEventMaps ++
        [ Widget.actionEventMapMovesCursor Config.addParamKeys "Add as Parameter" .
          liftM WidgetIds.fromIRef $ DataOps.addAsParameter newName definitionRef expressionI
        , Widget.actionEventMapMovesCursor Config.newDefinitionKeys "Add new as Definition" $
          liftM WidgetIds.fromIRef $ DataOps.addAsDefinition newName expressionI
        ]
    searchTermWidget <-
      (liftM . Widget.strongerEvents) searchTermEventMap $
      BWidgets.makeWordEdit stateProp searchTermId
    resultWidgets <- maybeResults firstResults
    moreResultsWidget <- mkMoreResultWidget
    return . BWidgets.vbox $ [searchTermWidget, resultWidgets] ++ moreResultsWidget

make ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  Widget.Id -> TWidget ViewTag m
make ancestry definitionI curState expressionPtr myId = do
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
