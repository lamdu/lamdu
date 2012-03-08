{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
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

data Expression m
  = ExpressionPlain (ExpressionPtr m)
  | ExpressionWhere (Where m)

getExpression :: MonadF m => ExpressionPtr m -> Transaction ViewTag m (Expression m)
getExpression exprPtr = do
  exprI <- Property.get exprPtr
  expr <- Transaction.readIRef exprI
  let plain = return $ ExpressionPlain exprPtr
  case expr of
    Data.ExpressionApply (Data.Apply funcI argI) -> do
      let
        argPtr =
          Property (return argI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . (Data.Apply funcI)
      func <- Transaction.readIRef funcI
      case func of
        Data.ExpressionLambda (Data.Lambda paramI bodyI) -> do
          let
            bodyPtr =
              Property (return bodyI) $
              Transaction.writeIRef funcI .
              Data.ExpressionLambda . (Data.Lambda paramI)
            item = WhereItem
              { wiParamI = paramI
              , wiExprPtr = argPtr
              , wiRemoveItem = do
                  Property.set exprPtr bodyI
                  return bodyI
              }
          sBody <- getExpression bodyPtr
          return . ExpressionWhere . atWWheres (item :) $ case sBody of
            ExpressionPlain innerBodyI -> Where [] innerBodyI
            ExpressionWhere x -> x
        _ -> plain
    _ -> plain
