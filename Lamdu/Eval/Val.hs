{-# LANGUAGE DeriveFunctor #-}
module Lamdu.Eval.Val
    ( ValHead, ValBody(..), ThunkId, Closure(..), Scope(..), ScopeId, emptyScope
    ) where

import Data.Map (Map)
import Lamdu.Data.Definition (FFIName)
import Lamdu.Expr.Val (Val)
import qualified Data.Map as Map
import qualified Lamdu.Expr.Val as V

data ValBody val pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend val)
    | HRecEmpty
    | HInteger Integer
    | HBuiltin FFIName
    deriving (Functor, Show)

type ValHead = ValBody ThunkId

type ThunkId = Int

data Closure pl = Closure
    { _cOuterScope :: Scope
    , _cLam :: V.Lam (Val pl)
    , _cLamPayload :: pl
    } deriving (Functor, Show)

data Scope = Scope
    { _scopeMap :: Map V.Var ThunkId
    , _scopeId :: ScopeId
    } deriving (Show)

type ScopeId = Int

emptyScope :: Scope
emptyScope = Scope Map.empty 0
