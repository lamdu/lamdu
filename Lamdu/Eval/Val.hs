{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Eval.Val
    ( ValHead, ValBody(..)
    , ThunkId(..), thunkIdInt
    , ScopeId(..), scopeIdInt
    , Closure(..), Scope(..)
    , emptyScope
    , _HFunc, _HRecExtend, _HRecEmpty, _HInteger, _HBuiltin
    , children
    ) where

import qualified Control.Lens as Lens
import           Data.Foldable (Foldable)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Traversable (Traversable)
import           Lamdu.Data.Definition (FFIName)
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V

newtype ThunkId = ThunkId { getThunkId :: Int }
    deriving (Show, Eq, Ord)

newtype ScopeId = ScopeId { getScopeId :: Int }
    deriving (Show, Eq, Ord)

thunkIdInt :: Lens.Iso' ThunkId Int
thunkIdInt = Lens.iso getThunkId ThunkId

data Scope = Scope
    { _scopeMap :: Map V.Var ThunkId
    , _scopeId :: ScopeId
    } deriving (Show)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data Closure pl = Closure
    { _cOuterScope :: Scope
    , _cLam :: V.Lam (Val pl)
    , _cLamPayload :: pl
    } deriving (Show, Functor, Foldable, Traversable)

data ValBody val pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend val)
    | HRecEmpty
    | HInteger Integer
    | HBuiltin FFIName
    deriving (Functor, Foldable, Traversable)

instance (Show pl, Show val) => Show (ValBody val pl) where
    show (HFunc closure) = show closure
    show (HRecExtend recExtend) = show recExtend
    show HRecEmpty = "{}"
    show (HInteger x) = show x
    show (HBuiltin ffiName) = show ffiName

Lens.makePrisms ''ValBody

children :: Lens.Traversal (ValBody a pl) (ValBody b pl) a b
children = _HRecExtend . Lens.traverse

type ValHead = ValBody ThunkId

emptyScope :: Scope
emptyScope = Scope Map.empty (ScopeId 0)
