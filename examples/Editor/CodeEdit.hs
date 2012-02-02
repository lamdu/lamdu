{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.List(intersperse, isInfixOf)
import Data.List.Utils(enumerate, removeAt)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget, readCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
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

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = BWidgets.hbox . intersperse spaceWidget

spaceView :: Sized Anim.Frame
spaceView = Spacer.makeHorizontal 20

spaceWidget :: Widget f
spaceWidget = Widget.liftView spaceView

makeActiveHoleEdit ::
  MonadF m => Data.HoleState -> IRef Data.Expression ->
  Anim.AnimId -> TWidget ViewTag m
makeActiveHoleEdit curState expressionI myId =
  assignCursor myId searchTermId $ do
    searchTermWidget <- BWidgets.makeTextEdit stateProp searchTermId
    vars <- mapM getVarName =<< getP Anchors.globals
    let
      results = filter goodResult vars
      (firstResults, moreResults) = splitAt 3 results
      mkMoreResultWidget
        | null moreResults = return []
        | otherwise = liftM (:[]) . BWidgets.makeTextView "..." $ Anim.joinId myId ["more results"]
    resultWidgets <- maybeResults firstResults
    moreResultsWidget <- mkMoreResultWidget
    return . BWidgets.vbox $ [searchTermWidget, resultWidgets] ++ moreResultsWidget
  where
    maybeResults [] = BWidgets.makeTextView "(No results)" $ Anim.joinId myId ["no results"]
    maybeResults xs = liftM BWidgets.vbox $ mapM makeResultWidget xs
    expressionId = AnimIds.fromIRef expressionI

    searchTermId = AnimIds.searchTermAnimId myId
    resultAnimId name = Anim.joinId myId ["search results", SBS8.pack name]
    makeResultWidget v@(_, name) =
      (liftM . Widget.strongerEvents) (pickResultEventMap v) .
      BWidgets.makeFocusableTextView name $ resultAnimId name
    pickResultEventMap (var, name) =
      EventMap.fromEventTypes Config.pickResultKeys "Pick this search result" $ do
        Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
        let
          -- TODO: Is there a better way?
          getVariableTextAnimId = expressionId
          mapAnimId animId =
            maybe animId (Anim.joinId getVariableTextAnimId) $ Anim.subId (resultAnimId name) animId

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

makeHoleEdit ::
  MonadF m => Data.HoleState -> IRef Data.Expression ->
  Anim.AnimId -> TWidget ViewTag m
makeHoleEdit curState expressionI myId = do
  cursor <- readCursor
  widget <-
    if isJust (Anim.subId myId cursor)
    then makeActiveHoleEdit curState expressionI myId
    else BWidgets.makeFocusableTextView snippet $ AnimIds.searchTermAnimId myId
  return $ Widget.backgroundColor (Anim.joinId myId ["hole background"]) holeBackgroundColor widget
  where
    holeBackgroundColor = Draw.Color 0.8 0 0 0.3
    snippet
      | null searchText = "-"
      | otherwise = searchText
    searchText = Data.holeSearchTerm curState

makeExpressionEdit :: MonadF m =>
  Bool -> Property (Transaction ViewTag m) (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Anim.AnimId)
makeExpressionEdit isArgument expressionPtr = do
  expressionI <- getP expressionPtr
  let
    diveIn = AnimIds.delegating . AnimIds.fromIRef
    expressionRef = Transaction.fromIRef expressionI
    mkCallWithArg = fmap diveIn . DataOps.callWithArg
    mkGiveAsArg = fmap diveIn $ DataOps.giveAsArg expressionPtr
    eventMap = mconcat
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" $
        mkCallWithArg expressionPtr
      ]
    weakerEvents = liftM . first . Widget.weakerEvents
    myId = AnimIds.fromIRef expressionI

    wrap keys entryState f =
      BWidgets.wrapDelegatedWithKeys keys entryState first f myId
    wrapTextEditor =
      wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating
    wrapExpr =
      wrap Config.exprFocusDelegatorKeys FocusDelegator.Delegating

    mkDelEvent = weakerEvents . Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
    mkCallWithArgEventMap =
      Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" $
      mkCallWithArg expressionPtr
    setExpr newExprI = do
      Property.set expressionPtr newExprI
      return $ AnimIds.fromIRef newExprI
  expr <- getP expressionRef
  widget <-
    case expr of
      Data.ExpressionHole holeState ->
        wrapTextEditor .
          (fmap . liftM) (flip (,) myId) $
          makeHoleEdit holeState expressionI
      Data.ExpressionGetVariable varRef -> do
        name <- getP $ Anchors.variableNameRef varRef
        varRefView <- BWidgets.makeFocusableTextView name myId
        let
          jumpToDefinitionEventMap =
            Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
          jumpToDefinition =
            case varRef of
              Data.DefinitionRef defI -> newPane defI
              Data.ParameterRef _paramI -> error "not yet"
              Data.BuiltinRef _builtI -> error "todo"
        return (Widget.weakerEvents jumpToDefinitionEventMap varRefView, myId)
      Data.ExpressionApply (Data.Apply funcI argI) ->
        wrapExpr $
          \animId ->
            assignCursor animId (AnimIds.fromIRef argI) $ do
              let
                funcIPtr =
                  Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
                argIPtr =
                  Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
              (funcEdit, funcAnimId) <- mkDelEvent argI $ makeExpressionEdit False funcIPtr
              (argEdit, _) <-
                 weakerEvents mkCallWithArgEventMap .
                 mkDelEvent funcI $ makeExpressionEdit True argIPtr
              let label str = BWidgets.makeTextView str $ Anim.joinId funcAnimId [pack str]
              before <- label "("
              after <- label ")"
              return
                ((BWidgets.hbox . concat)
                 [[before | isArgument],
                  [funcEdit], [spaceWidget], [argEdit],
                  [after | isArgument]]
                , funcAnimId)
  return .
    (first . Widget.weakerEvents . mconcat . concat) [
      [ mkCallWithArgEventMap | not isArgument ],
      [ eventMap ]
    ] $
    widget

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor animId nameEditAnimId $
    BWidgets.makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Anim.joinId animId ["equals"]
  (expressionEdit, _) <- makeExpressionEdit False bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params
  return .
    Widget.strongerEvents eventMap . hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.strongerEvents) (paramEventMap paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating (BWidgets.makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      AnimIds.fromIRef paramI
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    paramEventMap paramI =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete Parameter" .
      (liftM . const) animId $
      DataOps.delParameter definitionRef paramI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add Parameter" .
      liftM (AnimIds.delegating . AnimIds.fromIRef) $
      DataOps.addParameter definitionRef
    nameEditAnimId = Anim.joinId animId ["name"]
    animId = AnimIds.fromIRef definitionI

newPane :: Monad m => IRef Data.Definition -> Transaction ViewTag m Widget.Cursor
newPane defI = do
  Property.pureModify panesRef maybeAddPane
  return $ AnimIds.fromIRef defI
  where
    maybeAddPane panes
      | any ((== defI) . Anchors.paneDefinition) panes = panes
      | otherwise = Anchors.makePane defI : panes
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
      return . AnimIds.fromIRef . Anchors.paneDefinition . last $
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
      (firstPane:_) -> do
        assignCursor myId
          (AnimIds.fromIRef (Anchors.paneDefinition firstPane)) $ do
            definitionEdits <- mapM makePaneWidget $ enumerate panes
            return $ BWidgets.vboxAlign 0 definitionEdits

  return $ Widget.weakerEvents newDefinitionEventMap panesWidget
  where
    panesRef = Transaction.fromIRef Anchors.rootIRef
    myId = AnimIds.fromIRef Anchors.rootIRef
