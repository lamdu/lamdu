{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Arrow (first)
import Control.Monad (liftM, when)
import Data.ByteString.Char8 (pack)
import Data.List(intersperse, isInfixOf)
import Data.List.Utils(enumerate, removeAt)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget, readCursor, atTextStyle)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.WidgetIds as WidgetIds
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = BWidgets.hbox . intersperse spaceWidget

spaceView :: Sized Anim.Frame
spaceView = Spacer.makeHorizontal 20

spaceWidget :: Widget f
spaceWidget = Widget.liftView spaceView

setTextColor :: Draw.Color -> TWidget t m -> TWidget t m
setTextColor = atTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const

makeVarView :: MonadF m => Data.VariableRef -> Widget.Id -> TWidget t m
makeVarView var myId = do
  name <- getP $ Anchors.variableNameRef var
  let
    color =
      case var of
        Data.BuiltinRef _ -> Config.builtinColor
        Data.DefinitionRef _ -> Config.definitionColor
        Data.ParameterRef _ -> Config.parameterColor
  setTextColor color $
    BWidgets.makeFocusableTextView name myId

makeActiveHoleEdit ::
  MonadF m => IRef Data.Definition -> Data.HoleState -> IRef Data.Expression ->
  Widget.Id -> TWidget ViewTag m
makeActiveHoleEdit definitionI curState expressionI myId =
  assignCursor myId searchTermId $ do
    Data.Definition paramIs _ <- getP definitionRef
    searchTermWidget <-
      (liftM . Widget.strongerEvents) searchTermEventMap $
      BWidgets.makeTextEdit stateProp searchTermId
    globals <- getP Anchors.globals
    vars <- mapM getVarName $ map Data.ParameterRef paramIs ++ globals
    let
      results = filter goodResult vars
      (firstResults, moreResults) = splitAt 3 results
      mkMoreResultWidget
        | null moreResults = return []
        | otherwise = liftM (:[]) . BWidgets.makeTextView "..." $ Widget.joinId myId ["more results"]
    resultWidgets <- maybeResults firstResults
    moreResultsWidget <- mkMoreResultWidget
    return . BWidgets.vbox $ [searchTermWidget, resultWidgets] ++ moreResultsWidget
  where
    maybeResults [] = BWidgets.makeTextView "(No results)" $ Widget.joinId myId ["no results"]
    maybeResults xs = liftM BWidgets.vbox $ mapM (makeResultWidget . fst) xs
    expressionId = WidgetIds.fromIRef expressionI
    expressionAnimId = Widget.cursorId expressionId

    searchTermId = WidgetIds.searchTermId myId
    searchResultsId = Widget.joinId myId ["search results"]
    varId = Data.onVariableIRef WidgetIds.fromIRef
    makeResultWidget var =
      (liftM . Widget.strongerEvents) (pickResultEventMap var) .
      makeVarView var . mappend searchResultsId $ varId var
    pickResultEventMap var =
      EventMap.fromEventTypes Config.pickResultKeys "Pick this search result" $ do
        Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
        let
          -- TODO: Is there a better way?
          getVariableTextAnimId = expressionAnimId

          mapOtherResult resultId =
            ["mismatched result"]
              `Anim.joinId` expressionAnimId
              `Anim.joinId` resultId
          mapSearchResult resultId =
            maybe (mapOtherResult resultId)
              (Anim.joinId getVariableTextAnimId) $
            (Anim.subId . Widget.cursorId . varId) var resultId
          mapAnimId animId =
            maybe animId mapSearchResult $
            Anim.subId (Widget.cursorId searchResultsId) animId

        return Widget.EventResult {
          Widget.eCursor = Just expressionId,
          Widget.eAnimIdMapping = mapAnimId
          }
    stateProp =
      Property.Property {
        Property.get = return $ Data.holeSearchTerm curState,
        Property.set = Transaction.writeIRef expressionI . Data.ExpressionHole . (`Data.atHoleSearchTerm` curState) . const
      }
    getVarName var = liftM ((,) var) . getP $ Anchors.variableNameRef var
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
          newPane newDefI
      ]

makeHoleEdit ::
  MonadF m => IRef Data.Definition -> Data.HoleState ->
  IRef Data.Expression -> Widget.Id -> TWidget ViewTag m
makeHoleEdit definitionI curState expressionI myId = do
  cursor <- readCursor
  widget <-
    if isJust (Widget.subId myId cursor)
    then makeActiveHoleEdit definitionI curState expressionI myId
    else BWidgets.makeFocusableTextView snippet $ WidgetIds.searchTermId myId
  return $ Widget.backgroundColor (Widget.joinId myId ["hole background"]) holeBackgroundColor widget
  where
    holeBackgroundColor = Draw.Color 0.8 0 0 0.3
    snippet
      | null searchText = "-"
      | otherwise = searchText
    searchText = Data.holeSearchTerm curState

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap $ WidgetIds.delegating . WidgetIds.fromIRef

replace :: MonadF m => Transaction.Property t m (IRef Data.Expression) -> Transaction t m Widget.Id
replace = diveIn . DataOps.replace

makeExpressionEdit :: MonadF m =>
  Bool -> IRef Data.Definition ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeExpressionEdit isArgument definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    mkCallWithArg = diveIn $ DataOps.callWithArg expressionPtr
    mkGiveAsArg = diveIn $ DataOps.giveAsArg expressionPtr
    weakerEvents = liftM . first . Widget.weakerEvents
    expressionId = WidgetIds.fromIRef expressionI

    wrap keys entryState f =
      BWidgets.wrapDelegatedWithKeys keys entryState first f expressionId
    wrapTextEditor =
      wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating
    wrapExpr =
      wrap Config.exprFocusDelegatorKeys FocusDelegator.Delegating

    mkDelEvent = weakerEvents . Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
    mkCallWithArgEventMap =
      Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" mkCallWithArg
    setExpr newExprI = do
      Property.set expressionPtr newExprI
      return $ WidgetIds.fromIRef newExprI
  expr <- getP expressionRef
  widget <-
    case expr of
      Data.ExpressionHole holeState ->
        wrapTextEditor .
          (fmap . liftM) (flip (,) expressionId) $
          makeHoleEdit definitionI holeState expressionI
      Data.ExpressionGetVariable varRef -> do
        varRefView <- makeVarView varRef expressionId
        let
          jumpToDefinitionEventMap =
            Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
          jumpToDefinition =
            case varRef of
              Data.DefinitionRef defI -> newPane defI
              Data.ParameterRef paramI -> return $ WidgetIds.fromIRef paramI
              Data.BuiltinRef _builtI -> return expressionId
        return (Widget.weakerEvents jumpToDefinitionEventMap varRefView, expressionId)
      Data.ExpressionApply (Data.Apply funcI argI) ->
        wrapExpr $
          \myId ->
            assignCursor myId (WidgetIds.fromIRef argI) $ do
              let
                funcIPtr =
                  Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
                argIPtr =
                  Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
              (funcEdit, funcId) <- mkDelEvent argI $ makeExpressionEdit False definitionI funcIPtr
              (argEdit, _) <-
                 weakerEvents mkCallWithArgEventMap .
                 mkDelEvent funcI $ makeExpressionEdit True definitionI argIPtr
              let label str = BWidgets.makeTextView str $ Widget.joinId funcId [pack str]
              before <- label "("
              after <- label ")"
              return
                ((BWidgets.hbox . concat)
                 [[before | isArgument],
                  [funcEdit], [spaceWidget], [argEdit],
                  [after | isArgument]]
                , funcId)
  return .
    (first . Widget.weakerEvents . mconcat . concat) [
      [ mkCallWithArgEventMap | not isArgument ],
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" mkCallWithArg
      , Widget.actionEventMapMovesCursor
        Config.relinkKeys "Replace" $ replace expressionPtr ]
    ] $
    widget

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId $
    BWidgets.makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]
  (expressionEdit, _) <- makeExpressionEdit False definitionI bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params

  let replaceEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Replace" $ replace bodyRef
  return .
    Widget.weakerEvents eventMap . hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, Widget.weakerEvents replaceEventMap expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.weakerEvents) (paramEventMap paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (setTextColor Config.parameterColor .
       BWidgets.makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      WidgetIds.fromIRef paramI
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    paramEventMap paramI =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete Parameter" .
      (liftM . const) myId $
      DataOps.delParameter definitionRef paramI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add Parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter definitionRef
    nameEditAnimId = Widget.joinId myId ["name"]
    myId = WidgetIds.fromIRef definitionI

newPane :: Monad m => IRef Data.Definition -> Transaction ViewTag m Widget.Id
newPane defI = do
  panes <- Property.get panesRef
  when (all ((/= defI) . Anchors.paneDefinition) panes) $
    Property.set panesRef $ Anchors.makePane defI : panes
  return $ WidgetIds.fromIRef defI
  where
    panesRef = Transaction.fromIRef Anchors.rootIRef

makePanesEdit :: MonadF m => TWidget ViewTag m
makePanesEdit = do
  panes <- getP panesRef

  let
    newDefinitionEventMap =
      Widget.actionEventMapMovesCursor Config.newDefinitionKeys
        "New Definition" $ do
          newDefI <- Anchors.makeDefinition
          newPane newDefI

    delPane i = do
      let newPanes = removeAt i panes
      Property.set panesRef newPanes
      return . WidgetIds.fromIRef . Anchors.paneDefinition . last $
        take (i+1) newPanes

    paneEventMap (_:_:_) i =
      Widget.actionEventMapMovesCursor Config.closePaneKeys
        "Close Pane" $ delPane i
    paneEventMap _ _ = mempty

    makePaneWidget (i, pane) =
      (liftM . Widget.weakerEvents) (paneEventMap panes i) .
      makeDefinitionEdit $ Anchors.paneDefinition pane

  panesWidget <-
    case panes of
      [] -> BWidgets.makeFocusableTextView "<No panes>" myId
      (firstPane:_) ->
        assignCursor myId
          (WidgetIds.fromIRef (Anchors.paneDefinition firstPane)) $ do
            definitionEdits <- mapM makePaneWidget $ enumerate panes
            return $ BWidgets.vboxAlign 0 definitionEdits

  return $ Widget.weakerEvents newDefinitionEventMap panesWidget
  where
    panesRef = Transaction.fromIRef Anchors.rootIRef
    myId = WidgetIds.fromIRef Anchors.rootIRef
