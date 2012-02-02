{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makeDefinitionEdit) where

import Control.Arrow (first)
import Control.Monad(liftM)
import Data.ByteString.Char8 (pack)
import Data.List(intersperse, isInfixOf)
import Data.List.Utils(enumerate)
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
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = BWidgets.hbox . intersperse spaceWidget

spaceView :: Sized Anim.Frame
spaceView = Spacer.makeHorizontal 20

spaceWidget :: Widget f
spaceWidget = Widget.liftView spaceView

makeActiveHoleEdit :: MonadF m => Data.HoleState -> IRef Data.Expression -> TWidget ViewTag m
makeActiveHoleEdit curState expressionI =
  assignCursor myId searchTermId $ do
    searchTermWidget <- BWidgets.makeTextEdit stateProp searchTermId
    vars <- mapM getVarName =<< getP Anchors.globals
    resultWidgets <- mapM makeResultWidget . take 3 $ filter goodResult vars
    return . BWidgets.vbox $ searchTermWidget : resultWidgets
  where
    myId = AnimIds.fromIRef expressionI
    searchTermId = Anim.joinId myId ["search term"]
    makeResultWidget (var, name) =
      (liftM . Widget.strongerEvents)
        (Widget.actionEventMapMovesCursor Config.pickResultKeys "Pick this search result" (pickResult var)) $
      BWidgets.makeFocusableTextView name (Anim.joinId myId ["search results", SBS8.pack name])
    pickResult var = do
      Transaction.writeIRef expressionI $ Data.ExpressionGetVariable var
      return myId
    stateProp =
      Property.Property {
        Property.get = return $ Data.holeSearchTerm curState,
        Property.set = Transaction.writeIRef expressionI . Data.ExpressionHole . (`Data.atHoleSearchTerm` curState) . const
      }
    getVarName var = liftM ((,) var) . getP $ Anchors.variableNameRef var
    goodResult (_, name) = all (`isInfixOf` name) . words $ Data.holeSearchTerm curState

makeHoleEdit :: MonadF m => Data.HoleState -> IRef Data.Expression -> TWidget ViewTag m
makeHoleEdit curState expressionI = do
  cursor <- readCursor
  if isJust (Anim.subId myId cursor)
    then makeActiveHoleEdit curState expressionI
    else BWidgets.makeFocusableTextView ('<' : Data.holeSearchTerm curState ++ ">") myId
  where
    myId = AnimIds.fromIRef expressionI

makeExpressionEdit :: MonadF m =>
  Bool -> Property (Transaction ViewTag m) (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Anim.AnimId)
makeExpressionEdit isArgument expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    exprKeys = Config.exprFocusDelegatorKeys
    mkCallWithArg = fmap AnimIds.fromIRef . DataOps.callWithArg
    mkGiveAsArg = fmap AnimIds.fromIRef $ DataOps.giveAsArg expressionPtr
    eventMap = mconcat
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" $
        mkCallWithArg expressionPtr
      ]
    weakerEvents = liftM . first . Widget.weakerEvents
    wrap keys entryState f =
      (if isArgument then id else mkCallWithArgEvent) .
      weakerEvents eventMap .
      BWidgets.wrapDelegatedWithKeys keys entryState f first $
      AnimIds.fromIRef expressionI
    mkDelEvent = weakerEvents . Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
    mkCallWithArgEvent =
      weakerEvents . Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" $
      mkCallWithArg expressionPtr
    setExpr newExprI = do
      Property.set expressionPtr newExprI
      return $ AnimIds.fromIRef newExprI
  expr <- getP expressionRef
  case expr of
    Data.ExpressionHole holeState ->
      liftM (flip (,) (AnimIds.fromIRef expressionI)) $
      makeHoleEdit holeState expressionI
    Data.ExpressionGetVariable varI ->
      wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating .
        (fmap . liftM) (flip (,) (AnimIds.fromIRef expressionI)) .
        BWidgets.makeWordEdit $ Anchors.variableNameRef varI
    Data.ExpressionApply (Data.Apply funcI argI) ->
      wrap exprKeys FocusDelegator.Delegating $ \animId ->
        assignCursor animId (AnimIds.fromIRef argI) $ do
          let
            funcIPtr =
              Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
            argIPtr =
              Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
          (funcEdit, funcAnimId) <- mkDelEvent argI $ makeExpressionEdit False funcIPtr
          (argEdit, _) <- mkCallWithArgEvent . mkDelEvent funcI $ makeExpressionEdit True argIPtr
          let label str = BWidgets.makeTextView str $ Anim.joinId funcAnimId [pack str]
          before <- label "("
          after <- label ")"
          return
            ((BWidgets.hbox . concat) [[before | isArgument], [funcEdit], [spaceWidget], [argEdit], [after | isArgument]]
            ,funcAnimId)

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
