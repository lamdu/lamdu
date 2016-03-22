{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Eval.Val
    ( EvalError(..)
    , ScopeId(..), scopeIdInt, topLevelScopeId
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Lamdu.Data.Definition (FFIName)
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

newtype ScopeId = ScopeId { getScopeId :: Int }
    deriving (Show, Eq, Ord, Binary)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data EvalError
    = EvalHole
    | EvalTypeError String
    | EvalLoadGlobalFailed V.Var
    | EvalMissingBuiltin FFIName
    | EvalTodoError String
    | EvalIndexError String
    deriving Show

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0
