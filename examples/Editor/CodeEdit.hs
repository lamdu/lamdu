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
import qualified Data.Char as Char
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

data FuncType = Infix | Prefix
  deriving (Eq, Ord, Show, Read)

data ArgumentData m = ArgumentData {
  _adFuncType :: FuncType,
  adParentPtr :: Transaction.Property ViewTag m (IRef Data.Expression)
  }

data ExpressionAncestry m =
    Argument (ArgumentData m)
  | NotArgument
  | Root

isArgument :: ExpressionAncestry m -> Bool
isArgument (Argument _) = True
isArgument _ = False

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
  MonadF m => ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState -> Transaction.Property ViewTag m (IRef Data.Expression) ->
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
    varId = Data.onVariableIRef WidgetIds.fromIRef
    makeResultWidget v@(isInfix, var) =
      (liftM . Widget.strongerEvents) (pickResultEventMap v) .
      (if isInfix == Just False then (>>= addParens (varId var)) else id) .
      makeVarView var . mappend searchResultsId .
      (if isInfix == Just True then (`Widget.joinId` ["infix"]) else id) $
      varId var
    pickResultEventMap var =
      mconcat [
        EventMap.fromEventTypes Config.pickResultKeys "Pick this search result" $ pickResult var,
        EventMap.fromEventTypes Config.addNextArgumentKeys "Pick this search result and add argument" $ pickResultAndAddArg var
        ]

    pickResultAndAddArg v@(_, var) = do
      res <- pickResult v
      Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
      cursor <-
        diveIn $
        case ancestry of
          Argument (ArgumentData _ parentPtr) -> DataOps.callWithArg parentPtr
          _ -> DataOps.callWithArg expressionPtr
      return res { Widget.eCursor = Just cursor }

    pickResult (isInfix, var) = do
      Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
      when (isInfix == Just True) $ do
        let Argument argData = ancestry
        parentI <- Property.get $ adParentPtr argData
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
          (Anim.subId . Widget.cursorId . varId) var resultId

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
    processResult (var, name)
      | isOperatorName name && isArgument ancestry = [(Just True, var), (Just False, var)]
      | otherwise = [(Nothing, var)]

  assignCursor myId searchTermId $ do
    Data.Definition paramIs _ <- getP definitionRef
    globals <- getP Anchors.globals
    vars <- mapM getVarName $ map Data.ParameterRef paramIs ++ globals
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

makeHoleEdit ::
  MonadF m => ExpressionAncestry m ->
  IRef Data.Definition -> Data.HoleState ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  Widget.Id -> TWidget ViewTag m
makeHoleEdit ancestry definitionI curState expressionPtr myId = do
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

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap $ WidgetIds.delegating . WidgetIds.fromIRef

replace :: MonadF m => Transaction.Property t m (IRef Data.Expression) -> Transaction t m Widget.Id
replace = diveIn . DataOps.replace

makeAddNextArgEventMap :: MonadF m =>
  Transaction.Property t m (IRef Data.Expression) -> Widget.EventHandlers (Transaction t m)
makeAddNextArgEventMap =
  Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" .
  diveIn . DataOps.callWithArg

isOperatorName :: String -> Bool
isOperatorName = all (not . Char.isAlphaNum)

isInfixVar :: Monad m => Data.VariableRef -> CTransaction t m Bool
isInfixVar = liftM isOperatorName . getP . Anchors.variableNameRef

isInfixFunc :: Monad m => IRef Data.Expression -> CTransaction t m Bool
isInfixFunc funcI = do
  expr <- getP $ Transaction.fromIRef funcI
  case expr of
    Data.ExpressionGetVariable var -> isInfixVar var
    _ -> return False

makeApplyExpressionEdit :: MonadF m =>
  IRef Data.Definition -> Transaction.Property ViewTag m (IRef Data.Expression) ->
  Data.Apply -> Widget.Id ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeApplyExpressionEdit definitionI expressionPtr (Data.Apply funcI argI) myId =
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    expressionI <- getP expressionPtr
    isInfix <- isInfixFunc funcI
    let
      funcType
        | isInfix = Infix
        | otherwise = Prefix
      expressionRef = Transaction.fromIRef expressionI
      delEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
      funcIPtr = Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
      argIPtr = Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
      setExpr newExprI = do
        Property.set expressionPtr newExprI
        return $ WidgetIds.fromIRef newExprI
      addNextArgEventMap = makeAddNextArgEventMap expressionPtr
      funcEvents =
        Widget.weakerEvents (delEventMap argI) .
        if isInfix
        then Widget.strongerEvents addNextArgEventMap
        else id
    (funcEdit, parenId) <-
      (liftM . first) funcEvents $ makeExpressionEdit NotArgument definitionI funcIPtr
    (argEdit, _) <-
       (liftM . first . Widget.weakerEvents . mconcat)
       [ addNextArgEventMap
       , delEventMap funcI
       ] $ makeExpressionEdit (Argument (ArgumentData funcType expressionPtr)) definitionI argIPtr
    return
      ((BWidgets.hbox . if isInfix then reverse else id)
       [funcEdit, spaceWidget, argEdit], parenId)

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> CTransaction t m Bool
isApplyOfInfixOp exprI = do
  expr <- getP $ Transaction.fromIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> isInfixFunc funcI
    _ -> return False

makeExpressionEdit :: MonadF m =>
  ExpressionAncestry m -> IRef Data.Definition ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeExpressionEdit ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    mkCallWithArg = diveIn $ DataOps.callWithArg expressionPtr
    mkGiveAsArg = diveIn $ DataOps.giveAsArg expressionPtr
    expressionId = WidgetIds.fromIRef expressionI

    wrap keys entryState f =
      BWidgets.wrapDelegatedWithKeys keys entryState first f expressionId

    eventMap = mconcat $
      [ makeAddNextArgEventMap expressionPtr | not $ isArgument ancestry ] ++
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" mkCallWithArg
      , Widget.actionEventMapMovesCursor
        Config.relinkKeys "Replace" $ replace expressionPtr
      ]

  expr <- getP expressionRef
  (needParen, (widget, parenId)) <-
    case expr of
      Data.ExpressionHole holeState ->
        liftM ((,) False) .
        wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating .
          (fmap . liftM) (flip (,) expressionId) $
          makeHoleEdit ancestry definitionI holeState expressionPtr
      Data.ExpressionGetVariable varRef -> do
        varRefView <- makeVarView varRef expressionId
        isInfix <- isInfixVar varRef
        let
          jumpToDefinitionEventMap =
            Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
          jumpToDefinition =
            case varRef of
              Data.DefinitionRef defI -> newPane defI
              Data.ParameterRef paramI -> return $ WidgetIds.fromIRef paramI
              Data.BuiltinRef _builtI -> return expressionId
          needParen =
            case ancestry of
              NotArgument -> False
              _ -> isInfix
        return
          (needParen,
           (Widget.weakerEvents jumpToDefinitionEventMap varRefView,
            expressionId))
      Data.ExpressionApply apply@(Data.Apply funcI _) -> do
        isFullOp <- isApplyOfInfixOp funcI
        isInfix <- isInfixFunc funcI
        result <-
          wrap Config.exprFocusDelegatorKeys FocusDelegator.Delegating $
          makeApplyExpressionEdit definitionI expressionPtr apply
        let
          needParen =
            case ancestry of
              Root -> isInfix
              Argument _ -> True
              NotArgument -> isFullOp
        return (needParen, result)

  (resultWidget, resultParenId) <-
    if needParen then do
      resWidget <- addParens parenId widget
      return (resWidget, expressionId)
    else
      return (widget, parenId)
  return (Widget.weakerEvents eventMap resultWidget, resultParenId)

addParens :: MonadF m => Widget.Id -> Widget (Transaction t m) -> TWidget t m
addParens parenId widget = do
  beforeParen <- label "("
  afterParen <- label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeTextView str $ Widget.joinId parenId [pack str]

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId $
    BWidgets.makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]
  (expressionEdit, _) <- makeExpressionEdit Root definitionI bodyRef
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
