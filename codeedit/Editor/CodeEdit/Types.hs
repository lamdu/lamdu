{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.Types(
  ExpressionPtr,
  ExpressionAncestry,
  AncestryItem(..), getAncestryParams,
  ApplyParent(..), atApRole, atApFuncType, atApApply, atApParentPtr,
  LambdaParent(..), atLpLambda, atLpParentPtr,
  ApplyRole(..),
  FuncType(..),
  parensPrefix, addParens,
  varId, diveIn, isInfixName,
  isInfixVar, isInfixFunc, isApplyOfInfixOp,
  makeAddArgHandler, makeParensId)
where

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget)
import Editor.DataOps(ExpressionPtr)
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

data LambdaParent m = LambdaParent {
  lpLambda :: Data.Lambda,
  lpParentPtr :: ExpressionPtr m
  }
AtFieldTH.make ''LambdaParent

data AncestryItem m =
    AncestryItemApply (ApplyParent m)
  | AncestryItemLambda (LambdaParent m)

type ExpressionAncestry m = [AncestryItem m]

getAncestryParams :: ExpressionAncestry m -> [IRef Data.Parameter]
getAncestryParams ancestryItems =
  [ paramI
  | AncestryItemLambda (LambdaParent (Data.Lambda paramI _) _) <- ancestryItems ]

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

-- Return the target function to add "next arg" for
addNextArgTargetExpression
  :: MonadF m
  => ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction.Property ViewTag m (IRef Data.Expression)
addNextArgTargetExpression (AncestryItemApply (ApplyParent ApplyArg Prefix _ parentPtr) : _) _ = parentPtr
addNextArgTargetExpression (AncestryItemApply (ApplyParent ApplyFunc InfixLeft _ parentPtr) : _) _ = parentPtr
addNextArgTargetExpression _ expressionPtr = expressionPtr

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
       diveIn . DataOps.callWithArg $ addNextArgTargetExpression ancestry expressionPtr)

getNextArg :: ExpressionAncestry m -> Maybe (IRef Data.Expression)
getNextArg (
  AncestryItemApply (ApplyParent ApplyFunc _ apply _) :
  _) = Just $ Data.applyArg apply
getNextArg (
  AncestryItemApply (ApplyParent ApplyArg Prefix _ _) :
  AncestryItemApply (ApplyParent ApplyFunc _ parentApply _) :
  _) = Just $ Data.applyArg parentApply
getNextArg _ = Nothing

makeParensId :: Monad m => ExpressionAncestry m -> Transaction ViewTag m Widget.Id
makeParensId (AncestryItemApply (ApplyParent role _ _ parentPtr) : _) = do
  parentI <- Property.get parentPtr
  return $
    Widget.joinId (WidgetIds.fromIRef parentI)
    [pack $ show role]
makeParensId (AncestryItemLambda (LambdaParent _ parentPtr) : _) =
  liftM WidgetIds.fromIRef $ Property.get parentPtr
makeParensId [] =
  return $ Widget.Id ["root parens"]
