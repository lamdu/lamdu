{-# LANGUAGE TemplateHaskell #-}
module Editor.CodeEdit.FFI (Env(..), table) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Editor.Data as Data

data Env = Env
  { trueDef :: Data.DefinitionIRef
  , falseDef :: Data.DefinitionIRef
  }
derive makeBinary ''Env

class FromExpr a where
  fromExpr :: Env -> Data.PureExpression -> a

class ToExpr a where
  toExpr :: Env -> a -> [Data.PureExpression] -> Data.PureExpression

-- After we get rid of guids in pure expressions, this will be
-- simpler:
toPureExpr
  :: Data.ExpressionBody Data.PureExpression -> Data.PureExpression
toPureExpr = Data.pureExpression $ Guid.fromString "ZeroGuid"

instance FromExpr Integer where
  fromExpr _ (Data.Expression { Data.eValue = Data.ExpressionLeaf (Data.LiteralInteger x) }) = x
  fromExpr _ _ = error "Expecting normalized Integer expression!"

instance ToExpr Integer where
  toExpr _ x [] = toPureExpr . Data.ExpressionLeaf $ Data.LiteralInteger x
  toExpr _ _ _ = error "Integer applied as a function"

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr _ _ [] = error "Expecting more arguments"
  toExpr env f (x:xs) = (toExpr env . f . fromExpr env) x xs

instance ToExpr Bool where
  toExpr env True [] = toPureExpr . Data.makeDefinitionRef $ trueDef env
  toExpr env False [] = toPureExpr . Data.makeDefinitionRef $ falseDef env
  toExpr _ _ _ = error "Bool applied as a function"

instance FromExpr Bool where
  fromExpr env (Data.Expression { Data.eValue = Data.ExpressionLeaf (Data.GetVariable (Data.DefinitionRef defRef)) })
    | defRef == trueDef env = True
    | defRef == falseDef env = False
  fromExpr _ _ = error "Expected a normalized bool expression!"

table :: Env -> Map Data.FFIName ([Data.PureExpression] -> Data.PureExpression)
table env =
  Map.fromList
  [ prelude "==" ((==) :: Integer -> Integer -> Bool)
  , prelude "+" ((+) :: Integer -> Integer -> Integer)
  , prelude "-" ((-) :: Integer -> Integer -> Integer)
  , prelude "*" ((*) :: Integer -> Integer -> Integer)
  ]
  where
    prelude name val =
      (Data.FFIName ["Prelude"] name, toExpr env val)
