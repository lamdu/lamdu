{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Editor.Data (
  Definition(..), atDefParameters, atDefBody,
  Variable(..),
  GetVariable(..), atGetVarName,
  Apply(..), atApplyFunc, atApplyArg,
  Expression(..))
where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.IRef (IRef)
import qualified Data.AtFieldTH as AtFieldTH

data Variable = Variable
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
  defParameters :: [IRef Variable],
  defBody :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)

derive makeBinary ''Definition
derive makeBinary ''Apply
derive makeBinary ''GetVariable
derive makeBinary ''Expression
derive makeBinary ''Variable
AtFieldTH.make ''Definition
AtFieldTH.make ''Apply
AtFieldTH.make ''GetVariable
