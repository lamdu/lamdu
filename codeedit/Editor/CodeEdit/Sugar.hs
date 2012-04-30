{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), ExpressionRef(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
  , Func(..), atFParams, atFBody
  , FuncParam(..)
  , Apply(..)
  , ParensType(..)
  , convertExpression
  ) where

import Data.Store.IRef(IRef)
import Data.Store.Property(Property(Property))
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import Editor.DataOps(ExpressionPtr)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps

data ParensType = TextParens | SquareParens

data ExpressionRef m = ExpressionRef
  { rExpressionPtr :: ExpressionPtr m
  , rMParensType :: Maybe ParensType
  , rExpression :: Expression m
  }

data WhereItem m = WhereItem
  { wiParamI :: IRef Data.Parameter
  -- TODO: Show type as well ?
  , wiValue :: ExpressionRef m
  -- Pointer to original apply provided to still give access to it.
  , wiApplyPtr :: ExpressionPtr m
  -- IRef to body of lambda provided to allow deleting the where item.
  , wiLambdaBodyI :: IRef Data.Expression
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParam m = FuncParam
  { fpParamI :: IRef Data.Parameter
  , fpType :: ExpressionRef m
  -- Pointer to original lambda expression provided to still give access to the lambda.
  , fpLambdaPtr :: ExpressionPtr m
  -- IRef to original body of lambda provided to allow deleting the param/lambda.
  , fpBodyI :: IRef Data.Expression
  }

-- Multi-param Lambda
data Func m = Func
  { fParams :: [FuncParam m]
  , fBody :: ExpressionRef m
  }

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  }

data Expression m
  = ExpressionApply (Apply m)
  | ExpressionGetVariable Data.VariableRef
  | ExpressionHole Data.HoleState
  | ExpressionLiteralInteger Integer
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

AtFieldTH.make ''Where
AtFieldTH.make ''Func

data ParenState = ParensNeverNeeded | ApplyFunc | ApplyArg

convertLambda :: Monad m => ParenState -> ExpressionPtr m -> Data.Lambda -> Transaction ViewTag m (ExpressionRef m)
convertLambda parenState exprPtr lambda = do
  exprI <- Property.get exprPtr
  let
    typePtr = DataOps.lambdaParamTypeRef exprI lambda
    bodyPtr = DataOps.lambdaBodyRef exprI lambda
  typeExpr <- convertNode ParensNeverNeeded typePtr
  sBody <- convertNode ParensNeverNeeded bodyPtr
  let
    item = FuncParam
      { fpParamI = Data.tpParam (Data.lambdaParam lambda)
      , fpType = typeExpr
      , fpLambdaPtr = exprPtr
      , fpBodyI = Data.lambdaBody lambda
      }
    mParenType =
      case parenState of
      ApplyArg -> Just TextParens
      ApplyFunc -> error "redex should not be handled by convertLambda"
      _ -> Nothing
  return . ExpressionRef exprPtr mParenType . ExpressionFunc . atFParams (item :) $ case rExpression sBody of
    ExpressionFunc x -> x
    _ -> Func [] sBody

convertWhere
  :: Monad m
  => ParenState
  -> ExpressionPtr m
  -> IRef Data.Expression
  -> ExpressionPtr m
  -> Data.Lambda
  -> Transaction ViewTag m (ExpressionRef m)
convertWhere parenState argPtr funcI exprPtr lambda@(Data.Lambda (Data.TypedParam paramI _) bodyI) = do
  value <- convertNode ParensNeverNeeded argPtr
  let
    bodyPtr = DataOps.lambdaBodyRef funcI lambda
    item = WhereItem
      { wiParamI = paramI
      , wiValue = value
      , wiApplyPtr = exprPtr
      , wiLambdaBodyI = bodyI
      }
    mParenType =
      case parenState of
      ParensNeverNeeded -> Nothing
      _ -> Just SquareParens
  sBody <- convertNode ParensNeverNeeded bodyPtr
  return . ExpressionRef exprPtr mParenType . ExpressionWhere . atWWheres (item :) $ case rExpression sBody of
    ExpressionWhere x -> x
    _ -> Where [] sBody

convertApply
  :: Monad m
  => ParenState
  -> Property (Transaction ViewTag m) (IRef Data.Expression)
  -> Data.Apply
  -> Transaction ViewTag m (ExpressionRef m)
convertApply parenState exprPtr (Data.Apply funcI argI) = do
  exprI <- Property.get exprPtr
  let
    argPtr =
      Property (return argI) $
      Transaction.writeIRef exprI .
      Data.ExpressionApply . Data.Apply funcI
  func <- Transaction.readIRef funcI
  case func of
    Data.ExpressionLambda lambda ->
      convertWhere parenState argPtr funcI exprPtr lambda
    _ -> do
      let
        funcPtr =
          Property (return funcI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . (`Data.Apply` argI)
        mParenType =
          case parenState of
          ApplyArg -> Just TextParens
          _ -> Nothing
      funcExpr <- convertNode ApplyFunc funcPtr
      argExpr <- convertNode ApplyArg argPtr
      return . ExpressionRef exprPtr mParenType . ExpressionApply $ Apply funcExpr argExpr

convertGetVariable :: Monad m => ExpressionPtr m -> Data.VariableRef -> Transaction ViewTag m (ExpressionRef m)
convertGetVariable exprPtr = return . ExpressionRef exprPtr Nothing . ExpressionGetVariable

convertHole :: Monad m => ExpressionPtr m -> Data.HoleState -> Transaction ViewTag m (ExpressionRef m)
convertHole exprPtr = return . ExpressionRef exprPtr Nothing . ExpressionHole

convertLiteralInteger :: Monad m => ExpressionPtr m -> Integer -> Transaction ViewTag m (ExpressionRef m)
convertLiteralInteger exprPtr = return . ExpressionRef exprPtr Nothing . ExpressionLiteralInteger

convertNode :: Monad m => ParenState -> ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)
convertNode parenState exprPtr = do
  exprI <- Property.get exprPtr
  expr <- Transaction.readIRef exprI
  case expr of
    Data.ExpressionLambda x -> convertLambda parenState exprPtr x
    Data.ExpressionApply x -> convertApply parenState exprPtr x
    Data.ExpressionGetVariable x -> convertGetVariable exprPtr x
    Data.ExpressionHole x -> convertHole exprPtr x
    Data.ExpressionLiteralInteger x -> convertLiteralInteger exprPtr x

convertExpression :: Monad m => ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)
convertExpression = convertNode ParensNeverNeeded
