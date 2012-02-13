{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM2, (<=<))
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
import qualified Editor.CodeEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.CodeEdit.VarEdit as VarEdit
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

needParen ::
  Monad m => Data.Expression -> ETypes.ExpressionAncestry m ->
  CTransaction ViewTag m Bool
needParen (Data.ExpressionGetVariable _) ETypes.NotArgument =
  return False
needParen (Data.ExpressionGetVariable varRef) _ =
  ETypes.isInfixVar varRef
needParen (Data.ExpressionHole _) _ =
  return False
needParen (Data.ExpressionLiteralInteger _) _ =
  return False
needParen (Data.ExpressionApply (Data.Apply funcI _)) (ETypes.Argument argData) = do
  let
    apply = ETypes.adApply argData
    leftFuncI = Data.applyFunc apply
  insideInfix <- case ETypes.adFuncType argData of
    ETypes.Infix -> return True
    ETypes.Prefix -> ETypes.isApplyOfInfixOp leftFuncI
  isInfix <- liftM2 (||) (ETypes.isInfixFunc funcI) (ETypes.isApplyOfInfixOp funcI)
  return $ not insideInfix || isInfix
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
  exprNeedParen <- needParen expr ancestry
  let
    addParens (widget, parenId)
      | exprNeedParen =
        do
          resWidget <- ETypes.addParens parenId widget
          return (resWidget, expressionId)
      | otherwise = return (widget, parenId)

    makeEditor =
      case expr of
        Data.ExpressionHole holeState ->
          BWidgets.wrapDelegatedWithKeys
            FocusDelegator.defaultKeys FocusDelegator.Delegating first $
          HoleEdit.make ancestry definitionI holeState expressionPtr
        Data.ExpressionGetVariable varRef ->
          addParens <=< VarEdit.make varRef
        Data.ExpressionApply apply ->
          BWidgets.wrapDelegatedWithKeys
            Config.exprFocusDelegatorKeys FocusDelegator.Delegating first $
          addParens <=<
          ApplyEdit.make (flip make definitionI) expressionPtr apply
        Data.ExpressionLiteralInteger integer ->
          BWidgets.wrapDelegatedWithKeys
            FocusDelegator.defaultKeys FocusDelegator.NotDelegating first $
          addParens <=<
          LiteralEdit.makeInt expressionI integer
  (widget, parenId) <- makeEditor expressionId

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
  return (Widget.weakerEvents eventMap widget, parenId)
