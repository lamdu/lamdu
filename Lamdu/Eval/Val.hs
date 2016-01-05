{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Eval.Val
    ( EvalError(..)
    , Val(..)
    , ScopeId(..), scopeIdInt, topLevelScopeId
    , Closure(..), Scope(..)
    , emptyScope
    , _HFunc, _HRecExtend, _HCase, _HRecEmpty
    , _HAbsurd, _HLiteral, _HBuiltin, _HInject
    , extractField
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Data.Definition (FFIName)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

newtype ScopeId = ScopeId { getScopeId :: Int }
    deriving (Show, Eq, Ord, Binary)

data Scope srcId = Scope
    { _scopeMap :: Map V.Var (Val srcId)
    , _scopeId :: ScopeId
    } deriving (Show, Functor, Foldable, Traversable)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data Closure srcId = Closure
    { _cOuterScope :: Scope srcId
    , _cLam :: V.Lam (V.Val srcId)
    , _cLamPayload :: srcId
    } deriving (Functor, Foldable, Traversable)
instance Show (Closure srcId) where show _ = "Closure{..}"

data EvalError
    = EvalHole
    | EvalTypeError String
    | EvalLoadGlobalFailed V.GlobalId
    | EvalMissingBuiltin FFIName
    | EvalTodoError String
    deriving Show

data Val srcId
    = HFunc (Closure srcId)
    | HRecExtend (V.RecExtend (Val srcId))
    | HRecEmpty
    | HAbsurd
    | HCase (V.Case (Val srcId))
    | HLiteral V.Literal
    | HBuiltin FFIName
    | HInject (V.Inject (Val srcId))
    | HError EvalError
    deriving (Functor, Foldable, Traversable)

instance Show (Val srcId) where
    show (HFunc closure) = show closure
    show (HRecExtend recExtend) = show recExtend
    show (HCase case_) = show case_
    show (HInject inject) = show inject
    show HRecEmpty = "()"
    show HAbsurd = "Absurd"
    show (HError err) = show err
    show (HLiteral l) = show l
    show (HBuiltin ffiName) = show ffiName

Lens.makePrisms ''Val

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

emptyScope :: Scope srcId
emptyScope = Scope Map.empty topLevelScopeId

extractField :: T.Tag -> Val srcId -> Val srcId
extractField _ (HError err) = HError err
extractField tag (HRecExtend (V.RecExtend vt vv vr))
    | vt == tag = vv
    | otherwise = extractField tag vr
extractField tag x =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show x
    & EvalTypeError & HError
