{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Eval.Val
    ( Val(..)
    , ScopeId(..), scopeIdInt, topLevelScopeId
    , Closure(..), Scope(..)
    , emptyScope
    , _HFunc, _HRecExtend, _HCase, _HRecEmpty
    , _HAbsurd, _HInteger, _HBuiltin, _HInject
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Data.Definition (FFIName)
import qualified Lamdu.Expr.Val as V

newtype ScopeId = ScopeId { getScopeId :: Int }
    deriving (Show, Eq, Ord, Binary)

data Scope pl = Scope
    { _scopeMap :: Map V.Var (Val pl)
    , _scopeId :: ScopeId
    } deriving (Show, Functor, Foldable, Traversable)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data Closure pl = Closure
    { _cOuterScope :: Scope pl
    , _cLam :: V.Lam (V.Val pl)
    , _cLamPayload :: pl
    } deriving (Show, Functor, Foldable, Traversable)

data Val pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend (Val pl))
    | HRecEmpty
    | HAbsurd
    | HCase (V.Case (Val pl))
    | HInteger Integer
    | HBuiltin FFIName
    | HInject (V.Inject (Val pl))
    deriving (Functor, Foldable, Traversable)

instance Show pl => Show (Val pl) where
    show (HFunc closure) = show closure
    show (HRecExtend recExtend) = show recExtend
    show (HCase case_) = show case_
    show (HInject inject) = show inject
    show HRecEmpty = "{}"
    show HAbsurd = "Absurd"
    show (HInteger x) = show x
    show (HBuiltin ffiName) = show ffiName

Lens.makePrisms ''Val

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

emptyScope :: Scope pl
emptyScope = Scope Map.empty topLevelScopeId
