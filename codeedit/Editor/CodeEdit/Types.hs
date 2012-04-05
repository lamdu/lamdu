{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.Types(
  ExpressionPtr,
  ExpressionAncestry,
  AncestryItem(..), getAncestryParams,
  ApplyParent(..), atApRole, atApFuncType, atApApply, atApParentPtr,
  ApplyRole(..),
  LambdaParent(..), atLpFunc, atLpParentI,
  WhereParent(..), atWpWhere, atWpRole,
  WhereRole(..),
  FuncType(..),
  ExpressionEditMaker,
  parensPrefix, addParens,
  varId, diveIn, isInfixName,
  isInfixVar, isInfixFunc, isApplyOfInfixOp, infixFuncOfRArg,
  makeAddArgHandler, makeParensId, isAncestryRHS)
where

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust)
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
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

data FuncType = Prefix | InfixLeft | InfixRight
  deriving (Eq, Ord, Show, Read)

data ApplyRole = ApplyFunc | ApplyArg
  deriving (Show, Read, Eq, Ord)

data ApplyParent m = ApplyParent
  { apRole :: ApplyRole
  , apFuncType :: FuncType
  , apApply :: Data.Apply
  , apParentPtr :: ExpressionPtr m
  }
AtFieldTH.make ''ApplyParent

data LambdaParent m = LambdaParent
  { lpFunc :: Sugar.Func m
  , lpParentI :: IRef Data.Expression
  }
AtFieldTH.make ''LambdaParent

data WhereRole
  = WhereBody
  | WhereDef (IRef Data.Parameter)
  deriving (Show, Read, Eq, Ord)

data WhereParent m = WhereParent
  { wpWhere :: Sugar.Where m
  , wpRole :: WhereRole
  }
AtFieldTH.make ''WhereParent

data AncestryItem m =
    AncestryItemApply (ApplyParent m)
  | AncestryItemLambda (LambdaParent m)
  | AncestryItemWhere (WhereParent m)

type ExpressionAncestry m = [AncestryItem m]

type ExpressionEditMaker m =
  ExpressionAncestry m -> ExpressionPtr m -> TWidget ViewTag m

getAncestryParams :: ExpressionAncestry m -> [IRef Data.Parameter]
getAncestryParams =
  concatMap params
  where
    params (AncestryItemLambda (LambdaParent (Sugar.Func items _) _)) = map Sugar.fpParamI items
    params (AncestryItemWhere (WhereParent (Sugar.Where items _) _)) = map Sugar.wiParamI items
    params _ = []

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
  expr <- Transaction.readIRef funcI
  case expr of
    Data.ExpressionGetVariable var -> isInfixVar var
    _ -> return False

infixFuncOfRArg
  :: Monad m
  => IRef Data.Expression
  -> Transaction t m (Maybe (IRef Data.Expression))
infixFuncOfRArg exprI = do
  expr <- Transaction.readIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> do
      res <- isInfixFunc funcI
      if res
        then return (Just funcI)
        else return Nothing
    _ -> return Nothing

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> Transaction t m Bool
isApplyOfInfixOp = liftM isJust . infixFuncOfRArg

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
makeParensId (AncestryItemLambda (LambdaParent _ parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId (AncestryItemWhere (WhereParent (Sugar.Where _ body) role) : _) = do
  bodyI <- Property.get body
  return $
    Widget.joinId (WidgetIds.fromIRef bodyI)
    [pack $ show role]
makeParensId [] =
  return $ Widget.Id ["root parens"]

isAncestryRHS :: ExpressionAncestry m -> Bool
isAncestryRHS [AncestryItemLambda _] = True
isAncestryRHS (AncestryItemLambda _ : AncestryItemWhere _ : _) = True
isAncestryRHS _ = False
