{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
  , Func(..), atFParams, atFBody
  , FuncParam(..), funcParamOfLambda
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
  , wiExprPtr :: ExpressionPtr m
  , wiRemoveItem :: Transaction ViewTag m (IRef Data.Expression)
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionPtr m
  }
AtFieldTH.make ''Where

data FuncParam m = FuncParam
  { fpParamI :: IRef Data.Parameter
  , fpLambdaI :: IRef Data.Expression
  , fpRemoveParam :: Transaction ViewTag m (IRef Data.Expression)
  }

-- Multi-param Lambda
data Func m = Func
  { fParams :: [FuncParam m]
  , fBody :: ExpressionPtr m
  }
AtFieldTH.make ''Func

data Expression m
  = ExpressionPlain (IRef Data.Expression)
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

funcParamOfLambda :: Monad m => ExpressionPtr m -> Data.Lambda -> Transaction ViewTag m (FuncParam m)
funcParamOfLambda exprPtr (Data.Lambda paramI _ bodyI) = do
  exprI <- Property.get exprPtr
  return
    FuncParam
    { fpParamI = paramI
    , fpLambdaI = exprI
    , fpRemoveParam = do
        Property.set exprPtr bodyI
        return bodyI
    }

getExpression :: MonadF m => ExpressionPtr m -> Transaction ViewTag m (Expression m)
getExpression exprPtr = do
  exprI <- Property.get exprPtr
  expr <- Transaction.readIRef exprI
  let plain = return $ ExpressionPlain exprI
  case expr of
    Data.ExpressionLambda lambda -> do
      let bodyPtr = DataOps.lambdaBodyRef exprI lambda
      item <- funcParamOfLambda exprPtr lambda
      sBody <- getExpression bodyPtr
      return . ExpressionFunc . atFParams (item :) $ case sBody of
        ExpressionFunc x -> x
        _ -> Func [] bodyPtr
    Data.ExpressionApply (Data.Apply funcI argI) -> do
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
              , wiExprPtr = argPtr
              , wiRemoveItem = do
                  Property.set exprPtr bodyI
                  return bodyI
              }
          sBody <- getExpression bodyPtr
          return . ExpressionWhere . atWWheres (item :) $ case sBody of
            ExpressionWhere x -> x
            _ -> Where [] bodyPtr
        _ -> plain
    _ -> plain
