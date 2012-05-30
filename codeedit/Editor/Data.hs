{-# LANGUAGE TemplateHaskell, Rank2Types, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Editor.Data
  ( Definition(..), DefinitionBody(..)
  , FFIName(..)
  , VariableRef(..), variableRefGuid
  , Lambda(..), atLambdaParamType, atLambdaBody
  , Apply(..), atApplyFunc, atApplyArg
  , Expression(..)
  )
where

import Control.Applicative (pure, liftA2)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef(IRef)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.IRef as IRef

data Lambda i = Lambda {
  lambdaParamType :: i (Expression i),
  lambdaBody :: i (Expression i)
  }

instance Binary (i (Expression i)) => Binary (Lambda i) where
  get = liftA2 Lambda get get
  put (Lambda x y) = put x >> put y

data Apply i = Apply {
  applyFunc :: i (Expression i),
  applyArg :: i (Expression i)
  }

data VariableRef
  = ParameterRef Guid -- of the lambda/pi
  | DefinitionRef (IRef (Definition IRef))

data Expression i
  = ExpressionLambda (Lambda i)
  | ExpressionPi (Lambda i)
  | ExpressionApply (Apply i)
  | ExpressionGetVariable VariableRef
  | ExpressionHole
  | ExpressionLiteralInteger Integer

instance Binary (i (Expression i)) => Binary (Apply i) where
  get = liftA2 Apply get get
  put (Apply x y) = put x >> put y

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord, Read, Show)

data DefinitionBody i
  = DefinitionExpression (i (Expression i))
  | DefinitionBuiltin FFIName
  | DefinitionMagic

data Definition i = Definition
  { defType :: i (Expression i)
  , defBody :: DefinitionBody i
  }

instance Binary (i (Expression i)) => Binary (DefinitionBody i) where
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap DefinitionExpression get
      1 -> fmap DefinitionBuiltin get
      2 -> return DefinitionMagic
      _ -> fail "Invalid tag in serialization of Definition"
  put (DefinitionExpression x) = putWord8 0 >> put x
  put (DefinitionBuiltin x) = putWord8 1 >> put x
  put DefinitionMagic = putWord8 2

instance Binary (i (Expression i)) => Binary (Definition i) where
  get = liftA2 Definition get get
  put (Definition x y) = put x >> put y

instance Binary VariableRef where
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap ParameterRef  get
      1 -> fmap DefinitionRef get
      _ -> fail "Invalid tag in serialization of VariableRef"
  put (ParameterRef x)  = putWord8 0 >> put x
  put (DefinitionRef x) = putWord8 1 >> put x

instance
  (Binary (i (Expression i)),
   Binary (i (Definition i)))
  => Binary (Expression i)
  where
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap ExpressionLambda         get
      1 -> fmap ExpressionPi             get
      2 -> fmap ExpressionApply          get
      3 -> fmap ExpressionGetVariable    get
      4 -> pure ExpressionHole
      5 -> fmap ExpressionLiteralInteger get
      _ -> fail "Invalid tag in serialization of Expression"
  put (ExpressionLambda x)         = putWord8 0 >> put x
  put (ExpressionPi x)             = putWord8 1 >> put x
  put (ExpressionApply x)          = putWord8 2 >> put x
  put (ExpressionGetVariable x)    = putWord8 3 >> put x
  put ExpressionHole               = putWord8 4
  put (ExpressionLiteralInteger x) = putWord8 5 >> put x

variableRefGuid :: VariableRef -> Guid
variableRefGuid (ParameterRef i) = i
variableRefGuid (DefinitionRef i) = IRef.guid i

derive makeBinary ''FFIName
AtFieldTH.make ''Lambda
AtFieldTH.make ''Apply
