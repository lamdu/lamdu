{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Data.Definition
  ( FFIName(..)
  , Builtin(..)
  , ExportedType(..), _ExportedType
  , Expr(..)
  , Body(..)
  , Definition(..), defBody, defPayload
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens)
import Data.Binary (Binary(..))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Scheme (Scheme)
import qualified Control.Lens as Lens

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

defBody :: Lens (Definition s a) (Definition t a) (Body s) (Body t)
defBody f (Definition b p) = (`Definition` p) <$> f b

defPayload :: Lens (Definition expr a) (Definition expr b) a b
defPayload f (Definition b p) = Definition b <$> f p

_ExportedType :: Lens.Prism' ExportedType Scheme
_ExportedType =
  Lens.prism' ExportedType f
  where
    f (ExportedType t) = Just t
    f _ = Nothing
