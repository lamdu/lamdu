{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM, liftM2)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.CodeEdit.VarEdit as VarEdit
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

makeHoleEdit ::
  MonadF m =>
  ETypes.ExpressionAncestry m -> IRef Data.Definition ->
  Data.HoleState -> ETypes.ExpressionPtr m ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeHoleEdit ancestry definitionI holeState expressionPtr = do
  expressionId <- liftM WidgetIds.fromIRef $ getP expressionPtr
  BWidgets.wrapDelegatedWithKeys
    FocusDelegator.defaultKeys FocusDelegator.NotDelegating first
    ((fmap . liftM) (flip (,) expressionId) $
    HoleEdit.make ancestry definitionI holeState expressionPtr) expressionId

needParen ::
  Monad m => Data.Expression -> ETypes.ExpressionAncestry m ->
  CTransaction ViewTag m Bool
needParen (Data.ExpressionGetVariable _) ETypes.NotArgument =
  return False
needParen (Data.ExpressionGetVariable varRef) _ =
  ETypes.isInfixVar varRef
needParen (Data.ExpressionHole _) _ =
  return False
needParen (Data.ExpressionApply (Data.Apply funcI _)) (ETypes.Argument argData) = do
  let
    apply = ETypes.adApply argData
    leftFuncI = Data.applyFunc apply
  insideInfix <- case ETypes.adFuncType argData of
    ETypes.Infix -> return True
    ETypes.Prefix -> ETypes.isApplyOfInfixOp leftFuncI
  isInfix <- liftM2 (||) (ETypes.isInfixFunc funcI) (ETypes.isApplyOfInfixOp funcI)
  return $ isInfix == insideInfix
needParen (Data.ExpressionApply (Data.Apply funcI _)) ETypes.Root =
  ETypes.isInfixFunc funcI
needParen (Data.ExpressionApply (Data.Apply funcI _)) ETypes.NotArgument =
  ETypes.isApplyOfInfixOp funcI


make :: MonadF m =>
  ETypes.ExpressionAncestry m -> IRef Data.Definition ->
  ETypes.ExpressionPtr m ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionId = WidgetIds.fromIRef expressionI

  expr <- getP $ Transaction.fromIRef expressionI
  (widget, parenId) <-
    case expr of
      Data.ExpressionHole holeState ->
        makeHoleEdit ancestry definitionI holeState expressionPtr
      Data.ExpressionGetVariable varRef ->
        VarEdit.make expressionId varRef
      Data.ExpressionApply apply ->
        ApplyEdit.make (flip make definitionI) expressionPtr apply

  exprNeedParen <- needParen expr ancestry
  (resultWidget, resultParenId) <-
    if exprNeedParen then do
      resWidget <- ETypes.addParens parenId widget
      return (resWidget, expressionId)
    else
      return (widget, parenId)

  let
    eventMap = mconcat $
      [ ETypes.makeAddNextArgEventMap expressionPtr | not $ ETypes.isArgument ancestry ] ++
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument" .
        ETypes.diveIn $ DataOps.giveAsArg expressionPtr
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" . ETypes.diveIn $ DataOps.callWithArg expressionPtr
      , Widget.actionEventMapMovesCursor
        Config.relinkKeys "Replace" . ETypes.diveIn $ DataOps.replace expressionPtr
      ]
  return (Widget.weakerEvents eventMap resultWidget, resultParenId)
