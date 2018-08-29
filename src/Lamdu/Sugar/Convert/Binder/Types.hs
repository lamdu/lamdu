module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (DefI, ValP)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (V.Lam (Val (ValP m)))
    | BinderKindLambda
