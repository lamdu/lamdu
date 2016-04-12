module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (V.Lam (Val (ValIProperty m)))
    | BinderKindLambda
