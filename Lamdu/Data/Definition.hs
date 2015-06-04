{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Lamdu.Data.Definition
  ( FFIName(..)
  , Builtin(..)
  , ExportedType(..), _NoExportedType, _ExportedType
  , Expr(..), expr, exprType
  , Body(..), _BodyExpr, _BodyBuiltin
  , Definition(..), defBody, defPayload
  ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary(..))
import           Data.Foldable (Foldable(..))
import           Data.Traversable (Traversable(..))
import           GHC.Generics (Generic)
import           Lamdu.Expr.Scheme (Scheme)

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Generic, Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin = Builtin
  { bName :: FFIName
  , bType :: Scheme
  } deriving (Generic, Show)

data ExportedType = NoExportedType | ExportedType Scheme
  deriving (Generic, Show)

data Expr valExpr = Expr
  { _expr :: valExpr
  , _exprType :: ExportedType
  } deriving (Generic, Show, Functor, Foldable, Traversable)

data Body valExpr
  = BodyExpr (Expr valExpr)
  | BodyBuiltin Builtin
  deriving (Generic, Show, Functor, Foldable, Traversable)

data Definition valExpr a = Definition
  { _defBody :: Body valExpr
  , _defPayload :: a
  } deriving (Generic, Functor, Foldable, Traversable)

instance Binary FFIName
instance Binary Builtin
instance Binary ExportedType
instance Binary valExpr => Binary (Expr valExpr)
instance Binary valExpr => Binary (Body valExpr)
instance (Binary valExpr, Binary a) => Binary (Definition valExpr a)

Lens.makePrisms ''ExportedType
Lens.makePrisms ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''Expr
