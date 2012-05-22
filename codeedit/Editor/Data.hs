{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Editor.Data (
  Definition(..),
  Builtin(..), FFIName(..),
  VariableRef(..), onVariableIRef,
  Lambda(..), atLambdaParamType, atLambdaBody,
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

data Lambda = Lambda {
  lambdaParamType :: IRef Expression,
  lambdaBody :: IRef Expression
  } deriving (Eq, Ord, Read, Show)

data Apply = Apply {
  applyFunc :: IRef Expression,
  applyArg :: IRef Expression
  } deriving (Eq, Ord, Read, Show)

data Expression
  = ExpressionLambda Lambda
  | ExpressionPi Lambda
  | ExpressionApply Apply
  | ExpressionGetVariable VariableRef
  | ExpressionHole
  | ExpressionLiteralInteger Integer
  deriving (Eq, Ord, Read, Show)

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord, Read, Show)

data Builtin = Builtin {
  biName :: FFIName,
  biType :: IRef Expression
  } deriving (Eq, Ord, Read, Show)

data Definition
  = DefinitionExpression (IRef Expression)
  | DefinitionBuiltin Builtin
  deriving (Eq, Ord, Read, Show)

data VariableRef
  = ParameterRef (IRef Expression)
  | DefinitionRef (IRef Definition)
  deriving (Eq, Ord, Read, Show)

onVariableIRef :: (forall a. IRef a -> b) -> VariableRef -> b
onVariableIRef f (ParameterRef i) = f i
onVariableIRef f (DefinitionRef i) = f i

derive makeBinary ''Apply
derive makeBinary ''Lambda
derive makeBinary ''FFIName
derive makeBinary ''Builtin
derive makeBinary ''Definition
derive makeBinary ''Expression
derive makeBinary ''VariableRef
AtFieldTH.make ''Lambda
AtFieldTH.make ''Apply
