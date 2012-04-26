{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), ExpressionRef(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
  , Func(..), atFParams, atFBody
  , FuncParam(..)
  , Apply(..)
  , Parens(..), ParensInfo(..), HighlightParens(..)
  , convertExpression
  ) where

import Control.Monad (liftM)
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
import qualified Graphics.UI.Bottle.Widget as Widget

data ExpressionRef m = ExpressionRef
  { rExpressionPtr :: ExpressionPtr m
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

data HighlightParens = DoHighlightParens | DontHighlightParens

data ParensInfo
  = TextParens HighlightParens Widget.Id
  | SquareParens Widget.Id

data Parens m = Parens
  { parensInfo :: ParensInfo
  , parensBody :: ExpressionRef m
  }

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  }

data Expression m
  = ExpressionParens (Parens m)
  | ExpressionApply (Apply m)
  | ExpressionGetVariable Data.VariableRef
  | ExpressionHole Data.HoleState
  | ExpressionLiteralInteger Integer
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

AtFieldTH.make ''Where
AtFieldTH.make ''Func

convertLambda :: Monad m => ExpressionPtr m -> Data.Lambda -> Transaction ViewTag m (Expression m)
convertLambda exprPtr lambda = do
  exprI <- Property.get exprPtr
  let
    typePtr = DataOps.lambdaParamTypeRef exprI lambda
    bodyPtr = DataOps.lambdaBodyRef exprI lambda
  typeExpr <- convertExpression typePtr
  sBody <- convertExpression bodyPtr
  let
    item = FuncParam
      { fpParamI = Data.tpParam (Data.lambdaParam lambda)
      , fpType = typeExpr
      , fpLambdaPtr = exprPtr
      , fpBodyI = Data.lambdaBody lambda
      }

  return . ExpressionFunc . atFParams (item :) $ case rExpression sBody of
    ExpressionFunc x -> x
    _ -> Func [] sBody

convertApply
  :: Monad m
  => Property (Transaction ViewTag m) (IRef Data.Expression)
  -> Data.Apply
  -> Transaction ViewTag m (Expression m)
convertApply exprPtr (Data.Apply funcI argI) = do
  exprI <- Property.get exprPtr
  let
    argPtr =
      Property (return argI) $
      Transaction.writeIRef exprI .
      Data.ExpressionApply . Data.Apply funcI
  func <- Transaction.readIRef funcI
  case func of
    Data.ExpressionLambda lambda@(Data.Lambda (Data.TypedParam paramI _) bodyI) -> do
      value <- convertExpression argPtr
      let
        bodyPtr = DataOps.lambdaBodyRef funcI lambda
        item = WhereItem
          { wiParamI = paramI
          , wiValue = value
          , wiApplyPtr = exprPtr
          , wiLambdaBodyI = bodyI
          }
      sBody <- convertExpression bodyPtr
      return . ExpressionWhere . atWWheres (item :) $ case rExpression sBody of
        ExpressionWhere x -> x
        _ -> Where [] sBody
    _ -> do
      let
        funcPtr =
          Property (return funcI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . (`Data.Apply` argI)
      funcExpr <- convertExpression funcPtr
      argExpr <- convertExpression argPtr
      return . ExpressionApply $ Apply funcExpr argExpr

convertGetVariable :: Monad m => ExpressionPtr m -> Data.VariableRef -> Transaction ViewTag m (Expression m)
convertGetVariable _exprPtr = return . ExpressionGetVariable

convertHole :: Monad m => ExpressionPtr m -> Data.HoleState -> Transaction ViewTag m (Expression m)
convertHole _exprPtr = return . ExpressionHole

convertLiteralInteger :: Monad m => ExpressionPtr m -> Integer -> Transaction ViewTag m (Expression m)
convertLiteralInteger _exprPtr = return . ExpressionLiteralInteger

convertExpression :: Monad m => ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)
convertExpression exprPtr = do
  expr <- Transaction.readIRef =<< Property.get exprPtr
  liftM (ExpressionRef exprPtr) $ case expr of
    Data.ExpressionLambda x -> convertLambda exprPtr x
    Data.ExpressionApply x -> convertApply exprPtr x
    Data.ExpressionGetVariable x -> convertGetVariable exprPtr x
    Data.ExpressionHole x -> convertHole exprPtr x
    Data.ExpressionLiteralInteger x -> convertLiteralInteger exprPtr x
