{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( InferT, Context, Error(..), TypedValue(..), ScopedTypedValue(..)
  , infer
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
--import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (InferT)
-- import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferT

mergeVars :: MonadA m => RefVars -> RefVars -> InferT def m RefVars
mergeVars (RefVars aScope aGetVars) (RefVars bScope bGetVars)
  | any (`Map.notMember` aScope) (Set.toList bGetVars)
  || any (`Map.notMember` bScope) (Set.toList aGetVars)
  = InferT.inferError VarEscapesScope
  | otherwise
  = (`RefVars` Set.union aGetVars bGetVars)
    <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

mergeBodies ::
  (Eq def, MonadA m) =>
  Expr.Body def Ref ->
  Expr.Body def Ref ->
  InferT def m (Expr.Body def Ref)
mergeBodies a (Expr.BodyLeaf Expr.Hole) = return a
mergeBodies (Expr.BodyLeaf Expr.Hole) b = return b
mergeBodies a b = do
  case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther matchGetPar a b of
    Nothing -> InferT.inferError $ Mismatch a b
    Just mkBody -> mkBody
  where
    matchLamResult _aGuid _bGuid aRef bRef = do
      unify {-TODO-} aRef bRef
    matchOther = unify
    matchGetPar = error "TODO"

mergeRefData :: (Eq def, MonadA m) => RefData def -> RefData def -> InferT def m (RefData def)
mergeRefData (RefData aVars aBody) (RefData bVars bBody) =
  RefData <$> mergeVars aVars bVars <*> mergeBodies aBody bBody

unify :: (Eq def, MonadA m) => Ref -> Ref -> InferT def m Ref
unify = ExprRefs.unifyRefs mergeRefData

-- TODO: Shut ghc up
_unify :: (Eq def, MonadA m) => Ref -> Ref -> InferT def m Ref
_unify = unify

data TypedValue = TypedValue
  { tvVal :: {-# UNPACK #-}! Ref
  , tvType :: {-# UNPACK #-}! Ref
  } deriving (Eq, Ord)
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

data ScopedTypedValue = ScopedTypedValue
  { stvTV :: TypedValue
  , stvScope :: Map Guid Ref
  }

infer ::
  Expr.Expression (Ref, def) a ->
  InferT def m (Expr.Expression (Ref, def) (ScopedTypedValue, a))
infer _refExpr = do
  error "TODO"
