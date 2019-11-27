{-# LANGUAGE TypeOperators #-}
module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           Hyper (Ann, type (#))
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (DefI, HRef)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (V.Lam V.Var V.Term # Ann (HRef m))
    | BinderKindLambda
