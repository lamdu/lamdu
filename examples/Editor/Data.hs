{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Editor.Data (
  Definition(..), atDefParameters, atDefBody,
  Parameter(..), addParameter, delParameter,
  GetVariable(..), atGetVarName,
  Apply(..), atApplyFunc, atApplyArg,
  Expression(..))
where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.List (delete)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction

data Parameter = Parameter
  deriving (Eq, Ord, Read, Show)

data Apply = Apply {
  applyFunc :: IRef Expression,
  applyArg :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)

data GetVariable = GetVariable {
  getVarName :: IRef String
  }
  deriving (Eq, Ord, Read, Show)

data Expression = ExpressionApply Apply | ExpressionGetVariable GetVariable
  deriving (Eq, Ord, Read, Show)

data Definition = Definition {
  defParameters :: [IRef Parameter],
  defBody :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)

derive makeBinary ''Definition
derive makeBinary ''Apply
derive makeBinary ''GetVariable
derive makeBinary ''Expression
derive makeBinary ''Parameter
AtFieldTH.make ''Definition
AtFieldTH.make ''Apply
AtFieldTH.make ''GetVariable

addParameter ::
  Monad m => Transaction.Property t m Definition ->
  Transaction t m (IRef Parameter)
addParameter definitionRef = do
  newParamI <- Transaction.newIRef Parameter
  Property.pureModify definitionRef . atDefParameters $
    (++ [newParamI])
  return newParamI

delParameter ::
  Monad m => Transaction.Property t m Definition ->
  IRef Parameter -> Transaction t m ()
delParameter definitionRef paramI =
  Property.pureModify definitionRef . atDefParameters $
    delete paramI
