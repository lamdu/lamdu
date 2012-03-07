{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.Types(
  ExpressionPtr,
  ExpressionAncestry,
  ApplyParent(..), atApRole, atApFuncType, atApApply, atApParentPtr,
  ApplyRole(..),
  FuncType(..),
  parensPrefix, addParens,
  varId, diveIn, isInfixName,
  isInfixVar, isInfixFunc, isApplyOfInfixOp,
  makeAddArgHandler)
where

import Control.Monad (liftM)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Char as Char
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

type ExpressionPtr m = Transaction.Property ViewTag m (IRef Data.Expression)

data FuncType = Prefix | InfixLeft | InfixRight
  deriving (Eq, Ord, Show, Read)

data ApplyRole = ApplyFunc | ApplyArg
  deriving (Show, Read, Eq, Ord)

data ApplyParent m = ApplyParent {
  apRole :: ApplyRole,
  apFuncType :: FuncType,
  apApply :: Data.Apply,
  apParentPtr :: ExpressionPtr m
  }
AtFieldTH.make ''ApplyParent

type ExpressionAncestry m = [ApplyParent m]

parensPrefix :: Widget.Id -> Widget.Id
parensPrefix = flip Widget.joinId ["parens"]

addParens
  :: MonadF m
  => (TWidget t m -> TWidget t m)
  -> (TWidget t m -> TWidget t m)
  -> Widget.Id
  -> Widget (Transaction t m)
  -> TWidget t m
addParens onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeLabel str $ parensPrefix parenId

varId :: Data.VariableRef -> Widget.Id
varId = Data.onVariableIRef WidgetIds.fromIRef

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap $ WidgetIds.delegating . WidgetIds.fromIRef

isInfixName :: String -> Bool
isInfixName "" = False
isInfixName name = all (not . Char.isAlphaNum) name

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
  -> Transaction.Property ViewTag m (IRef Data.Expression)
addArgTargetExpression [] expressionPtr = expressionPtr
addArgTargetExpression (argData : _) expressionPtr =
  if isParent then apParentPtr argData else expressionPtr
  where
    isParent =
      case apRole argData of
        ApplyArg -> funcType == Prefix
        ApplyFunc -> funcType == InfixLeft
    funcType = apFuncType argData

makeAddArgHandler
  :: MonadF m
  => ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction ViewTag m (String, Transaction ViewTag m Widget.Id)
makeAddArgHandler ancestry expressionPtr =
  case getNextArg ancestry of
    Nothing -> return addArg
    Just holeCandidateI -> do
      holeCandidate <- Property.get $ Transaction.fromIRef holeCandidateI
      return $ case holeCandidate of
        Data.ExpressionHole _ -> ("Move to next arg", return (WidgetIds.fromIRef holeCandidateI))
        _ -> addArg
  where
    addArg =
      ("Add next arg",
       diveIn . DataOps.callWithArg $ addArgTargetExpression ancestry expressionPtr)

getNextArg :: ExpressionAncestry m -> Maybe (IRef Data.Expression)
getNextArg (
  ApplyParent ApplyFunc _ apply _ :
  _) = Just $ Data.applyArg apply
getNextArg (
  ApplyParent ApplyArg Prefix _ _ :
  ApplyParent ApplyFunc _ parentApply _ :
  _) = Just $ Data.applyArg parentApply
getNextArg _ = Nothing
