{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.FFI (Env(..), table) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef

data Env = Env
  { trueDef :: DataIRef.DefI
  , falseDef :: DataIRef.DefI
  }
derive makeBinary ''Env

class FromExpr a where
  fromExpr :: Env -> Data.Expression DataIRef.DefI () -> a

class ToExpr a where
  toExpr :: Env -> a -> [Data.Expression DataIRef.DefI ()] -> Data.Expression DataIRef.DefI ()

instance FromExpr Integer where
  fromExpr _ (Data.Expression { Data._eValue = Data.ExpressionLeaf (Data.LiteralInteger x) }) = x
  fromExpr _ _ = error "Expecting normalized Integer expression!"

instance ToExpr Integer where
  toExpr _ x [] = Data.pureExpression . Data.ExpressionLeaf $ Data.LiteralInteger x
  toExpr _ _ _ = error "Integer applied as a function"

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr _ _ [] = error "Expecting more arguments"
  toExpr env f (x:xs) = (toExpr env . f . fromExpr env) x xs

instance ToExpr Bool where
  toExpr env True [] = Data.pureExpression . Data.makeDefinitionRef $ trueDef env
  toExpr env False [] = Data.pureExpression . Data.makeDefinitionRef $ falseDef env
  toExpr _ _ _ = error "Bool applied as a function"

instance FromExpr Bool where
  fromExpr env (Data.Expression { Data._eValue = Data.ExpressionLeaf (Data.GetVariable (Data.DefinitionRef defRef)) })
    | defRef == trueDef env = True
    | defRef == falseDef env = False
  fromExpr _ _ = error "Expected a normalized bool expression!"

table :: Env -> Map Data.FFIName ([Data.Expression DataIRef.DefI ()] -> Data.Expression DataIRef.DefI ())
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
