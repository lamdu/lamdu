{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}

module Lamdu.Sugar.Props
    ( SugarExpr(..)
    , varRefUnfinished
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

class HTraversable t => SugarExpr t where
    isUnfinished :: t f -> Bool
    isUnfinished _ = False

    isForbiddenInLightLam :: t f -> Bool
    isForbiddenInLightLam = isUnfinished

    sugarExprRecursive :: RecMethod SugarExpr t
    default sugarExprRecursive :: DefRecMethod SugarExpr t
    sugarExprRecursive _ = Dict

instance Recursive SugarExpr where recurse = sugarExprRecursive . proxyArgument

instance SugarExpr (Const (i (TagChoice name o)))
instance SugarExpr (Const (TagRef name i o))
instance SugarExpr (PostfixFunc v name i o)
instance SugarExpr (HoleOpt v name i o)
instance SugarExpr (FragOpt v name i o)

instance SugarExpr (Const (GetVar name o)) where isUnfinished = Lens.has (Lens._Wrapped . varRefUnfinished)
instance SugarExpr (Assignment v name i o) where isUnfinished = Lens.anyOf (_BodyPlain . apBody) isUnfinished
instance SugarExpr (Else v name i o) where isUnfinished = Lens.anyOf _SimpleElse isUnfinished

instance SugarExpr (Function v name i o) where
    isForbiddenInLightLam = Lens.nullOf (fParams . _LhsVar . vIsNullParam . Lens.only True)

instance SugarExpr (Binder v name i o) where
    isUnfinished = isUnfinished . (^. bBody)
    isForbiddenInLightLam = isForbiddenInLightLam . (^. bBody)

instance SugarExpr (BinderBody v name i o) where
    isUnfinished = Lens.anyOf _BinderTerm isUnfinished
    isForbiddenInLightLam =
        -- Let expressions not allowed in light lambdas (would be anyOf if they were)
        Lens.allOf _BinderTerm isForbiddenInLightLam

instance SugarExpr (Term v name i o) where
    isUnfinished (BodyLeaf LeafHole{}) = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyLeaf (LeafGetVar x)) = isUnfinished (Const x)
    isUnfinished _ = False
    isForbiddenInLightLam (BodyLam f) = isForbiddenInLightLam (f ^. lamFunc)
    isForbiddenInLightLam x = isUnfinished x

varRefUnfinished :: Lens.Traversal' (GetVar name m) ()
varRefUnfinished = vForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)
