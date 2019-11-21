module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           Hyper (Tree, Ann)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (DefI, HRef)

data BinderKind m
    = BinderKindDef (DefI m) -- TODO: Top-level defs to fix
    | BinderKindLet (Tree (V.Lam V.Var V.Term) (Ann (HRef m)))
    | BinderKindLambda
