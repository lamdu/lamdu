{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}

module Lamdu.Sugar.Props
    ( SugarExpr(..)
    , binderVarRefUnfinished
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse (Recursive(..), proxyArgument)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

class HTraversable t => SugarExpr t where
    isUnfinished :: t f -> Bool
    isUnfinished _ = False

    isForbiddenInLightLam :: t f -> Bool
    isForbiddenInLightLam = isUnfinished

    sugarExprRecursive ::
        Proxy t -> Dict (HNodesConstraint t SugarExpr)
    default sugarExprRecursive ::
        HNodesConstraint t SugarExpr =>
        Proxy t -> Dict (HNodesConstraint t SugarExpr)
    sugarExprRecursive _ = Dict

instance Recursive SugarExpr where
    recurse = sugarExprRecursive . proxyArgument

instance SugarExpr (Const (GetVar name o))
instance SugarExpr (Const (TId name o))
instance SugarExpr (Const (i (TagChoice name o)))
instance SugarExpr (Const (TagRef name i o))
instance SugarExpr (PostfixFunc v name i o)
instance SugarExpr (FragOpt v name i o)

instance SugarExpr (Const (BinderVarRef name o)) where
    isUnfinished (Const x) = Lens.has binderVarRefUnfinished x

instance SugarExpr (Assignment v name i o) where
    isUnfinished (BodyPlain x) = isUnfinished (x ^. apBody)
    isUnfinished BodyFunction{} = False

instance SugarExpr (Else v name i o) where
    isUnfinished (SimpleElse x) = isUnfinished x
    isUnfinished ElseIf{} = False

instance SugarExpr (Function v name i o) where
    isForbiddenInLightLam = not . Lens.has (fParams . _NullParam)

instance SugarExpr (Binder v name i o) where
    isUnfinished = isUnfinished . (^. bBody)
    isForbiddenInLightLam = isForbiddenInLightLam . (^. bBody)

instance SugarExpr (BinderBody v name i o) where
    isUnfinished (BinderTerm x) = isUnfinished x
    isUnfinished BinderLet{} = False
    isForbiddenInLightLam BinderLet{} = True
    isForbiddenInLightLam (BinderTerm x) = isForbiddenInLightLam x

instance SugarExpr (Term v name i o) where
    isUnfinished (BodyLeaf LeafHole{}) = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyLeaf (LeafGetVar (GetBinder x))) = isUnfinished (Const x)
    isUnfinished _ = False
    isForbiddenInLightLam (BodyLam f) = isForbiddenInLightLam (f ^. lamFunc)
    isForbiddenInLightLam x = isUnfinished x

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished = bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)
