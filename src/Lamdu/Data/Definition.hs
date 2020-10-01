{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Definition
    ( FFIName(..)
    , Expr(..), expr, exprFrozenDeps
    , Body(..), _BodyExpr, _BodyBuiltin
    , Definition(..), defBody, defType, defPayload
    , pruneDefExprDeps
    ) where

import qualified Control.Lens as Lens
import           Data.Map.Extended (setMapIntersection)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Hyper
import           Lamdu.Calc.Definition (Deps, depsGlobalTypes, depsNominals)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Term)
import           Lamdu.Calc.Type (Scheme)

import           Lamdu.Prelude

data FFIName = FFIName
    { fModule :: [Text]
    , fName :: Text
    }
    deriving stock (Generic, Eq, Ord)
    deriving anyclass Binary

instance Show FFIName where
    show (FFIName path name) =
        mconcat (path <&> (<> ".")) <> name & Text.unpack

data Expr valExpr = Expr
    { _expr :: valExpr
    , _exprFrozenDeps :: Deps
    }
    deriving stock (Generic, Show, Functor, Foldable, Traversable, Eq, Ord)
    deriving anyclass Binary

data Body valExpr
    = BodyExpr (Expr valExpr)
    | BodyBuiltin FFIName
    deriving (Generic, Show, Functor, Foldable, Traversable, Eq)
    deriving anyclass Binary

data Definition valExpr a = Definition
    { _defBody :: Body valExpr
    , _defType :: Pure # Scheme
    , _defPayload :: a
    } deriving (Generic, Functor, Foldable, Traversable, Eq)
    deriving anyclass Binary

Lens.makePrisms ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''Expr

-- Prune dependencies of an Expr after edits.
pruneDefExprDeps :: Expr (Ann a # Term) -> Deps
pruneDefExprDeps defExpr =
    defExpr ^. exprFrozenDeps
    & depsGlobalTypes %~ setMapIntersection valVars
    & depsNominals %~ setMapIntersection valNoms
    where
        val = defExpr ^. expr
        valVars =
            (val & hflipped %~ hmap (\_ _ -> Const ()))
            ^.. ExprLens.valGlobals mempty
            & Set.fromList
        valNoms = val ^.. ExprLens.valNominals & Set.fromList
