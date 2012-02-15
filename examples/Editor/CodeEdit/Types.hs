{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Types(
  ExpressionPtr,
  ExpressionAncestry(..), ApplyData(..), ApplyRole(..),
  FuncType(..),
  addParens,
  varId, diveIn, isInfixName,
  isInfixVar, isInfixFunc, isApplyOfInfixOp,
  makeAddArgHandler)
where

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

type ExpressionPtr m = Transaction.Property ViewTag m (IRef Data.Expression)

data FuncType = Infix | Prefix
  deriving (Eq, Ord, Show, Read)

data ApplyRole = ApplyFunc | ApplyArg
  deriving (Show, Read, Eq, Ord)

data ApplyData m = ApplyData {
  adRole :: ApplyRole,
  adFuncType :: FuncType,
  adApply :: Data.Apply,
  adParentPtr :: Transaction.Property ViewTag m (IRef Data.Expression)
  }

data ExpressionAncestry m =
    ApplyChild (ApplyData m)
  | Root

addParens :: MonadF m => Widget.Id -> Widget (Transaction t m) -> TWidget t m
addParens parenId widget = do
  beforeParen <- label "("
  afterParen <- label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeTextView str $ Widget.joinId parenId [pack str]

varId :: Data.VariableRef -> Widget.Id
varId = Data.onVariableIRef WidgetIds.fromIRef

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap $ WidgetIds.delegating . WidgetIds.fromIRef

isInfixName :: String -> Bool
isInfixName = all (not . Char.isAlphaNum)

isInfixVar :: Monad m => Data.VariableRef -> Transaction t m Bool
isInfixVar = liftM isInfixName . Property.get . Anchors.variableNameRef

isInfixFunc :: Monad m => IRef Data.Expression -> Transaction t m Bool
isInfixFunc funcI = do
  expr <- Property.get $ Transaction.fromIRef funcI
  case expr of
    Data.ExpressionGetVariable var -> isInfixVar var
    _ -> return False

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> Transaction t m Bool
isApplyOfInfixOp exprI = do
  expr <- Property.get $ Transaction.fromIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> isInfixFunc funcI
    _ -> return False

addArgTargetExpression
  :: MonadF m
  => ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction ViewTag m (Transaction.Property ViewTag m (IRef Data.Expression))
addArgTargetExpression Root expressionPtr = return expressionPtr
addArgTargetExpression (ApplyChild argData) expressionPtr =
  case adRole argData of
  ApplyArg -> do
    isApplyOfInfix <- isApplyOfInfixOp funcI
    let isInfix = isApplyOfInfix || infixFunc
    return $ if isInfix then expressionPtr else adParentPtr argData
  ApplyFunc ->
    return $ if infixFunc then adParentPtr argData else expressionPtr
  where
    infixFunc = adFuncType argData == Infix
    (Data.Apply funcI _) = adApply argData

makeAddArgHandler
  :: MonadF m
  => ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction ViewTag m (String, Transaction ViewTag m Widget.Id)
makeAddArgHandler ancestry expressionPtr = do
  case ancestry of
    ApplyChild ApplyData { adRole = ApplyFunc, adApply = Data.Apply _ argI } -> do
      arg <- Property.get $ Transaction.fromIRef argI
      case arg of
        Data.ExpressionHole _ ->
          return ("Move to next arg", return (WidgetIds.fromIRef argI))
        _ ->
          return addArg
    _ -> return addArg
  where
    addArg =
      ("Add next arg",
       diveIn . DataOps.callWithArg =<< addArgTargetExpression ancestry expressionPtr)
