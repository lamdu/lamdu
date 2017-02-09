{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, OverloadedStrings #-}
module Lamdu.Data.Definition
    ( FFIName(..)
    , Expr(..), expr, exprFrozenDeps
    , Body(..), _BodyExpr, _BodyBuiltin
    , Definition(..), defBody, defType, defPayload
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary(..))
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Lamdu.Calc.Type.Scheme (Scheme)
import           Lamdu.Infer (Dependencies)

import           Lamdu.Prelude

data FFIName = FFIName
    { fModule :: [Text]
    , fName :: Text
    } deriving (Generic, Eq, Ord)

instance Show FFIName where
    show (FFIName path name) =
        mconcat (path <&> (<> ".")) <> name & Text.unpack

data Expr valExpr = Expr
    { _expr :: valExpr
    , _exprFrozenDeps :: Dependencies
    } deriving (Generic, Show, Functor, Foldable, Traversable)

data Body valExpr
    = BodyExpr (Expr valExpr)
    | BodyBuiltin FFIName
    deriving (Generic, Show, Functor, Foldable, Traversable)

data Definition valExpr a = Definition
    { _defBody :: Body valExpr
    , _defType :: Scheme
    , _defPayload :: a
    } deriving (Generic, Functor, Foldable, Traversable)

instance Binary FFIName
instance Binary valExpr => Binary (Expr valExpr)
instance Binary valExpr => Binary (Body valExpr)
instance (Binary valExpr, Binary a) => Binary (Definition valExpr a)

Lens.makePrisms ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''Expr
