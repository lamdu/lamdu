module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           AST (Ann, Tree)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (DefI, ValP)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (Tree (V.Lam V.Var V.Term) (Ann (ValP m)))
    | BinderKindLambda
