{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.FFI (Env(..), table) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef

data Env t = Env
  { trueDef :: DataIRef.DefI t
  , falseDef :: DataIRef.DefI t
  }
derive makeBinary ''Env

class FromExpr a where
  fromExpr :: Env t -> DataIRef.Expression t () -> a

class ToExpr a where
  toExpr :: Env t -> a -> [DataIRef.Expression t ()] -> DataIRef.Expression t ()

instance FromExpr Integer where
  fromExpr _ (Expression.Expression { Expression._eValue = Expression.ExpressionLeaf (Expression.LiteralInteger x) }) = x
  fromExpr _ _ = error "Expecting normalized Integer expression!"

instance ToExpr Integer where
  toExpr _ x [] = Expression.pureExpression . Expression.ExpressionLeaf $ Expression.LiteralInteger x
  toExpr _ _ _ = error "Integer applied as a function"

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr _ _ [] = error "Expecting more arguments"
  toExpr env f (x:xs) = (toExpr env . f . fromExpr env) x xs

instance ToExpr Bool where
  toExpr env True [] = Expression.pureExpression . Expression.makeDefinitionRef $ trueDef env
  toExpr env False [] = Expression.pureExpression . Expression.makeDefinitionRef $ falseDef env
  toExpr _ _ _ = error "Bool applied as a function"

instance FromExpr Bool where
  fromExpr env (Expression.Expression { Expression._eValue = Expression.ExpressionLeaf (Expression.GetVariable (Expression.DefinitionRef defRef)) })
    | defRef == trueDef env = True
    | defRef == falseDef env = False
  fromExpr _ _ = error "Expected a normalized bool expression!"

table :: Env t -> Map Definition.FFIName ([DataIRef.Expression t ()] -> DataIRef.Expression t ())
table env =
  Map.fromList
  [ prelude "==" ((==) :: Integer -> Integer -> Bool)
  , prelude "+" ((+) :: Integer -> Integer -> Integer)
  , prelude "-" ((-) :: Integer -> Integer -> Integer)
  , prelude "*" ((*) :: Integer -> Integer -> Integer)
  ]
  where
    prelude name val =
      (Definition.FFIName ["Prelude"] name, toExpr env val)
