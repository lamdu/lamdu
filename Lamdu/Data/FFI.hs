{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.FFI (Env(..), table) where

import Control.Lens.Operators
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.IRef as ExprIRef

data Env t = Env
  { trueDef :: ExprIRef.DefI t
  , falseDef :: ExprIRef.DefI t
  }
derive makeBinary ''Env

class FromExpr a where
  fromExpr :: Env t -> ExprIRef.Expression t () -> a

class ToExpr a where
  toExpr :: Env t -> a -> [ExprIRef.Expression t ()] -> ExprIRef.Expression t ()

instance FromExpr Integer where
  fromExpr _ e =
    fromMaybe (error "Expecting normalized Integer expression!") $
    e ^? ExprLens.exprLiteralInteger

instance ToExpr Integer where
  toExpr _ x [] = ExprUtil.pureLiteralInteger x
  toExpr _ _ _ = error "Integer applied as a function"

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr _ _ [] = error "Expecting more arguments"
  toExpr env f (x:xs) = (toExpr env . f . fromExpr env) x xs

instance ToExpr Bool where
  toExpr env b [] =
    ExprLens.pureExpr . ExprLens.bodyDefinitionRef #
    (if b then trueDef else falseDef) env
  toExpr _ _ _ = error "Bool applied as a function"

instance FromExpr Bool where
  fromExpr env expr =
    case expr ^? ExprLens.exprDefinitionRef of
    Just defRef
      | defRef == trueDef env -> True
      | defRef == falseDef env -> False
    _ -> error "Expected a normalized bool expression!"

table :: Env t -> Map Definition.FFIName ([ExprIRef.Expression t ()] -> ExprIRef.Expression t ())
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
