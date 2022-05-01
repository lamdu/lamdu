module Lamdu.Sugar.Convert.Binder.Types
    ( BinderKind(..)
    ) where

import           Hyper (HCompose)
import           Hyper.Type.Prune (Prune)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Expr.IRef (DefI, HRef)

import           Lamdu.Prelude

data BinderKind m
    = BinderKindDef (DefI m)
    | BinderKindLet (V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m))
    | BinderKindLambda
