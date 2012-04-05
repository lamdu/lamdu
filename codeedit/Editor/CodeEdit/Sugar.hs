{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
  , Func(..), atFParams, atFBody
  , FuncParam(..)
  , getExpression
  ) where

import Data.Store.IRef(IRef)
import Data.Store.Property(Property(Property))
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import Editor.DataOps(ExpressionPtr)
import Editor.MonadF(MonadF)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps

data WhereItem m = WhereItem
  { wiParamI :: IRef Data.Parameter
  , wiValuePtr :: ExpressionPtr m
  , wiApplyPtr :: ExpressionPtr m
  , wiLambdaBodyI :: IRef Data.Expression
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionPtr m
  }
AtFieldTH.make ''Where

data FuncParam m = FuncParam
  { fpParamI :: IRef Data.Parameter
  , fpLambdaPtr :: ExpressionPtr m
  , fpBodyI :: IRef Data.Expression
  }

-- Multi-param Lambda
data Func m = Func
  { fParams :: [FuncParam m]
  , fBody :: ExpressionPtr m
  }
AtFieldTH.make ''Func

data Expression m
  = ExpressionApply Data.Apply
  | ExpressionGetVariable Data.VariableRef
  | ExpressionHole Data.HoleState
  | ExpressionLiteralInteger Integer
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

getExpression :: MonadF m => ExpressionPtr m -> Transaction ViewTag m (Expression m)
getExpression exprPtr = do
  exprI <- Property.get exprPtr
  expr <- Transaction.readIRef exprI
  case expr of
    Data.ExpressionLambda lambda -> do
      let
        bodyPtr = DataOps.lambdaBodyRef exprI lambda
        item =
          FuncParam
          { fpParamI = Data.lambdaParam lambda
          , fpLambdaPtr = exprPtr
          , fpBodyI = Data.lambdaBody lambda
          }
      sBody <- getExpression bodyPtr
      return . ExpressionFunc . atFParams (item :) $ case sBody of
        ExpressionFunc x -> x
        _ -> Func [] bodyPtr
    -- Match apply-of-lambda(redex/where)
    Data.ExpressionApply apply@(Data.Apply funcI argI) -> do
      let
        argPtr =
          Property (return argI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . Data.Apply funcI
      func <- Transaction.readIRef funcI
      case func of
        Data.ExpressionLambda lambda@(Data.Lambda paramI _ bodyI) -> do
          let
            bodyPtr = DataOps.lambdaBodyRef funcI lambda
            item = WhereItem
              { wiParamI = paramI
              , wiValuePtr = argPtr
              , wiApplyPtr = exprPtr
              , wiLambdaBodyI = bodyI
              }
          sBody <- getExpression bodyPtr
          return . ExpressionWhere . atWWheres (item :) $ case sBody of
            ExpressionWhere x -> x
            _ -> Where [] bodyPtr
        _ -> return $ ExpressionApply apply
    Data.ExpressionGetVariable x -> return $ ExpressionGetVariable x
    Data.ExpressionHole x -> return $ ExpressionHole x
    Data.ExpressionLiteralInteger x -> return $ ExpressionLiteralInteger x
