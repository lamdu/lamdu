module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Expr.IRef (DefI, ValP)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (V.Lam (Val (ValP m)))
    | BinderKindLambda
