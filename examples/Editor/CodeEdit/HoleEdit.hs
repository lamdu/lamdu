{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.HoleEdit(make) where

import Control.Monad (liftM, when)
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
import qualified Editor.CodeEdit.VarView as VarView
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget

data ResultType = InfixOperator | PrefixOperator | NotOperator
  deriving (Eq, Ord, Read, Show)

data Result = Result {
  _resultType :: ResultType,
  resultVar :: Data.VariableRef
  }

getDefinitionParamRefs :: Data.Definition -> [Data.VariableRef]
getDefinitionParamRefs (Data.Definition { Data.defParameters = paramIs }) =
  map Data.ParameterRef paramIs

makeResultView ::
  (MonadF m) => Widget.Id -> Result ->
  CTransaction t m (Widget (Transaction t m))
makeResultView myId (Result typ var) =
  maybeAddParens typ .
  VarView.make var . mappend myId .
  maybeAddPrefixWrap typ $
  ETypes.varId var
  where
    maybeAddPrefixWrap PrefixOperator = (`Widget.joinId` ["prefix"])
    maybeAddPrefixWrap _ = id
    maybeAddParens PrefixOperator = (>>= ETypes.addParens (ETypes.varId var))
    maybeAddParens _ = id

makeActiveHoleEdit ::
  MonadF m => ETypes.ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState -> ETypes.ExpressionPtr m ->
  Widget.Id -> TWidget ViewTag m
makeActiveHoleEdit ancestry definitionI curState expressionPtr myId = do
  expressionI <- getP expressionPtr
  let
    expressionId = WidgetIds.fromIRef expressionI
    expressionAnimId = Widget.cursorId expressionId
    maybeResults [] = BWidgets.makeTextView "(No results)" $ Widget.joinId myId ["no results"]
    maybeResults xs = liftM BWidgets.vbox $ mapM makeResultWidget xs

    searchTermId = WidgetIds.searchTermId myId
    searchResultsId = Widget.joinId myId ["search results"]
    makeResultWidget result =
      (liftM . Widget.strongerEvents) (pickResultEventMap result) $
      makeResultView searchResultsId result
    pickResultEventMap var =
      mconcat [
        EventMap.fromEventTypes Config.pickResultKeys "Pick this search result" $ pickResult var,
        EventMap.fromEventTypes Config.addNextArgumentKeys "Pick this search result and add argument" $ pickResultAndAddArg var
        ]

    pickResultAndAddArg result = do
      res <- pickResult result
      Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ resultVar result
      cursor <-
        ETypes.diveIn $
        case ancestry of
          ETypes.Argument (ETypes.ArgumentData _ parentPtr) -> DataOps.callWithArg parentPtr
          _ -> DataOps.callWithArg expressionPtr
      return res { Widget.eCursor = Just cursor }

    pickResult (Result isInfix var) = do
      Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
      when (isInfix == InfixOperator) $ do
        let ETypes.Argument argData = ancestry
        parentI <- Property.get $ ETypes.adParentPtr argData
        Property.pureModify (Transaction.fromIRef parentI) flipArgs
      return Widget.EventResult {
        Widget.eCursor = Just expressionId,
        Widget.eAnimIdMapping = mapAnimId var
        }
    flipArgs (Data.ExpressionApply (Data.Apply x y)) = Data.ExpressionApply $ Data.Apply y x
    flipArgs _ = error "flipArgs expects func"

    mapAnimId var animId =
      maybe animId mapSearchResult $
      Anim.subId (Widget.cursorId searchResultsId) animId
      where
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

    stateProp =
      Property.Property {
        Property.get = return $ Data.holeSearchTerm curState,
        Property.set = Transaction.writeIRef expressionI . Data.ExpressionHole . (`Data.atHoleSearchTerm` curState) . const
      }
    goodResult (_, name) = all (`isInfixOf` name) . words $ Data.holeSearchTerm curState
    definitionRef = Transaction.fromIRef definitionI
    newName = concat . words $ Data.holeSearchTerm curState
    searchTermEventMap =
      mconcat
      [ Widget.actionEventMapMovesCursor Config.addParamKeys "Add as Parameter" $ do
          newParam <- DataOps.addParameter definitionRef
          Property.set (Anchors.aNameRef newParam) newName
          Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ Data.ParameterRef newParam
          return expressionId
      , Widget.actionEventMapMovesCursor Config.newDefinitionKeys "Add new as Definition" $ do
          newDefI <- Anchors.makeDefinition
          Property.set (Anchors.aNameRef newDefI) newName
          Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ Data.DefinitionRef newDefI
          Anchors.newPane newDefI
      ]
    processResult (var, name)
      | ETypes.isOperatorName name && ETypes.isArgument ancestry =
        [Result InfixOperator var, Result PrefixOperator var]
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
    searchTermWidget <-
      (liftM . Widget.strongerEvents . mconcat . concat)
        [map pickResultEventMap (take 1 firstResults),
         [searchTermEventMap]] $
      BWidgets.makeWordEdit stateProp searchTermId
    resultWidgets <- maybeResults firstResults
    moreResultsWidget <- mkMoreResultWidget
    return .
      BWidgets.vbox $ [searchTermWidget, resultWidgets] ++ moreResultsWidget

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
