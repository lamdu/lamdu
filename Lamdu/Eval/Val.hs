{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Eval.Val
    ( EvalError(..)
    , EvalResult, Val(..)
    , ScopeId(..), scopeIdInt, topLevelScopeId
    , Closure(..), Scope(..)
    , emptyScope
    , _HFunc, _HRecExtend, _HCase, _HRecEmpty
    , _HAbsurd, _HLiteral, _HBuiltin, _HInject
    , extractField
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
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
    { _scopeMap :: Map V.Var (EvalResult srcId)
    , _scopeId :: ScopeId
    } deriving (Show, Functor, Foldable, Traversable)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data Closure srcId = Closure
    { _cOuterScope :: Scope srcId
    , _cLam :: V.Lam (V.Val srcId)
    , _cLamPayload :: srcId
    } deriving (Show, Functor, Foldable, Traversable)

data EvalError
    = EvalHole
    | EvalTypeError String
    | EvalLoadGlobalFailed V.GlobalId
    | EvalMissingBuiltin FFIName
    | EvalTodoError String
    deriving Show

type EvalResult srcId = Either EvalError (Val srcId)

data Val srcId
    = HFunc (Closure srcId)
    | HRecExtend (V.RecExtend (EvalResult srcId))
    | HRecEmpty
    | HAbsurd
    | HCase (V.Case (EvalResult srcId))
    | HLiteral V.Literal
    | HBuiltin FFIName
    | HInject (V.Inject (EvalResult srcId))
    deriving (Functor, Foldable, Traversable)

instance Show srcId => Show (Val srcId) where
    show (HFunc closure) = show closure
    show (HRecExtend recExtend) = show recExtend
    show (HCase case_) = show case_
    show (HInject inject) = show inject
    show HRecEmpty = "()"
    show HAbsurd = "Absurd"
    show (HLiteral l) = show l
    show (HBuiltin ffiName) = show ffiName

Lens.makePrisms ''Val

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

emptyScope :: Scope srcId
emptyScope = Scope Map.empty topLevelScopeId

extractField :: T.Tag -> EvalResult srcId -> EvalResult srcId
extractField _ (Left err) = Left err
extractField tag (Right (HRecExtend (V.RecExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField tag vr
extractField tag (Right x) =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show (void x)
    & EvalTypeError & Left
