{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.List.Utils(enumerate, removeAt)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.CodeEdit.VarView as VarView
import qualified Editor.CodeEdit.HoleEdit as HoleEdit
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

replace :: MonadF m => Transaction.Property t m (IRef Data.Expression) -> Transaction t m Widget.Id
replace = ETypes.diveIn . DataOps.replace

makeAddNextArgEventMap :: MonadF m =>
  Transaction.Property t m (IRef Data.Expression) -> Widget.EventHandlers (Transaction t m)
makeAddNextArgEventMap =
  Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" .
  ETypes.diveIn . DataOps.callWithArg

isInfixVar :: Monad m => Data.VariableRef -> CTransaction t m Bool
isInfixVar = liftM ETypes.isOperatorName . getP . Anchors.variableNameRef

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
        | isInfix = ETypes.Infix
        | otherwise = ETypes.Prefix
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
      (liftM . first) funcEvents $ makeExpressionEdit ETypes.NotArgument definitionI funcIPtr
    (argEdit, _) <-
       (liftM . first . Widget.weakerEvents . mconcat)
       [ addNextArgEventMap
       , delEventMap funcI
       ] $ makeExpressionEdit (ETypes.Argument (ETypes.ArgumentData funcType expressionPtr)) definitionI argIPtr
    return
      ((BWidgets.hbox . if isInfix then reverse else id)
       [funcEdit, BWidgets.spaceWidget, argEdit], parenId)

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> CTransaction t m Bool
isApplyOfInfixOp exprI = do
  expr <- getP $ Transaction.fromIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> isInfixFunc funcI
    _ -> return False

makeExpressionEdit :: MonadF m =>
  ETypes.ExpressionAncestry m -> IRef Data.Definition ->
  Transaction.Property ViewTag m (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeExpressionEdit ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    mkCallWithArg = ETypes.diveIn $ DataOps.callWithArg expressionPtr
    mkGiveAsArg = ETypes.diveIn $ DataOps.giveAsArg expressionPtr
    expressionId = WidgetIds.fromIRef expressionI

    wrap keys entryState f =
      BWidgets.wrapDelegatedWithKeys keys entryState first f expressionId

    eventMap = mconcat $
      [ makeAddNextArgEventMap expressionPtr | not $ ETypes.isArgument ancestry ] ++
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
          HoleEdit.make ancestry definitionI holeState expressionPtr
      Data.ExpressionGetVariable varRef -> do
        varRefView <- VarView.make varRef expressionId
        isInfix <- isInfixVar varRef
        let
          jumpToDefinitionEventMap =
            Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
          jumpToDefinition =
            case varRef of
              Data.DefinitionRef defI -> Anchors.newPane defI
              Data.ParameterRef paramI -> return $ WidgetIds.fromIRef paramI
              Data.BuiltinRef _builtI -> return expressionId
          needParen =
            case ancestry of
              ETypes.NotArgument -> False
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
              ETypes.Root -> isInfix
              ETypes.Argument _ -> True
              ETypes.NotArgument -> isFullOp
        return (needParen, result)

  (resultWidget, resultParenId) <-
    if needParen then do
      resWidget <- ETypes.addParens parenId widget
      return (resWidget, expressionId)
    else
      return (widget, parenId)
  return (Widget.weakerEvents eventMap resultWidget, resultParenId)

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId $
    BWidgets.makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]
  (expressionEdit, _) <- makeExpressionEdit ETypes.Root definitionI bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params

  let replaceEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Replace" $ replace bodyRef
  return .
    Widget.weakerEvents eventMap . BWidgets.hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, Widget.weakerEvents replaceEventMap expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.weakerEvents) (paramEventMap paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.setTextColor Config.parameterColor .
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

makePanesEdit :: MonadF m => TWidget ViewTag m
makePanesEdit = do
  panes <- getP panesRef

  let
    newDefinitionEventMap =
      Widget.actionEventMapMovesCursor Config.newDefinitionKeys
        "New Definition" $ do
          newDefI <- Anchors.makeDefinition
          Anchors.newPane newDefI

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
