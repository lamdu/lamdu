{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
    ( Expression(..)
    , Where(..), atWWheres, atWBody
    , getExpression
    ) where

import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.MonadF (MonadF)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

data Where = Where
    { wWheres :: [(IRef Data.Parameter, IRef Data.Expression)]
    , wBody :: IRef Data.Expression
    }
AtFieldTH.make ''Where

data Expression
    = ExpressionPlain (IRef Data.Expression)
    | ExpressionWhere Where

getExpression :: MonadF m => IRef Data.Expression -> Transaction t m Expression
getExpression exprI = do
  expr <- Transaction.readIRef exprI
  let plain = return $ ExpressionPlain exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI argI) -> do
      func <- Transaction.readIRef funcI
      case func of
        Data.ExpressionLambda (Data.Lambda paramI bodyI) -> do
          sBody <- getExpression bodyI
          return . ExpressionWhere . atWWheres ((paramI, argI) :) $ case sBody of
            ExpressionPlain innerBodyI -> Where [] innerBodyI
            ExpressionWhere x -> x
        _ -> plain
    _ -> plain
