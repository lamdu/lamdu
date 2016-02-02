module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import Lamdu.Expr.IRef (DefI)

data BinderKind m
    = BinderKindDef (DefI m)
    | BinderKindLet
    | BinderKindLambda
