{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, OverloadedStrings #-}
module Lamdu.Data.Definition
    ( FFIName(..)
    , Builtin(..)
    , ExportedType(..), _NoExportedType, _ExportedType
    , Expr(..), expr, exprType, exprUsedDefinitions
    , Body(..), _BodyExpr, _BodyBuiltin
    , Definition(..), defBody, defPayload
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Binary (Binary(..))
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Lamdu.Calc.Type.Scheme (Scheme)
import           Lamdu.Calc.Val (Var)

import           Prelude.Compat

data FFIName = FFIName
    { fModule :: [Text]
    , fName :: Text
    } deriving (Generic, Eq, Ord)

instance Show FFIName where
    show (FFIName path name) =
        mconcat (path <&> (<> ".")) <> name & Text.unpack

data Builtin = Builtin
    { bName :: FFIName
    , bType :: Scheme
    } deriving (Generic, Show)

data ExportedType = NoExportedType | ExportedType Scheme
    deriving (Generic, Show)

data Expr valExpr = Expr
    { _expr :: valExpr
    , _exprType :: ExportedType
    , _exprUsedDefinitions :: Map Var Scheme
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
