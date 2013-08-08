{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap, scopeParamRefs

  , UFExprs

  , RefData(..), rdScope, rdBody, rdWasNotDirectlyTag, rdTriggers
    , defaultRefData
  , fresh, freshHole
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Lamdu.Data.Infer.RefData
import Lamdu.Data.Infer.RefTags (ExprRef)
import qualified Control.Lens as Lens

-- TypedValue:
data TypedValue def = TypedValue
  { _tvVal :: {-# UNPACK #-}! (ExprRef def)
  , _tvType :: {-# UNPACK #-}! (ExprRef def)
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show (TypedValue def) where
  showsPrec n (TypedValue v t) =
    showParen (n > 0) (unwords [show v, ":", show t] ++)

-- ScopedTypedValue
data ScopedTypedValue def = ScopedTypedValue
  { _stvTV :: TypedValue def
  , _stvScope :: Scope def
  }
Lens.makeLenses ''ScopedTypedValue
