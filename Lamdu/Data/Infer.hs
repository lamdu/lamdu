{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( InferT, Context, Error(..), TypedValue(..), ScopedTypedValue(..)
  , infer
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (InferT)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferT

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

renameRefVars :: Map Guid Guid -> RefVars -> RefVars
renameRefVars renames (RefVars scope getVars) =
  RefVars (Map.mapKeys mapping scope) (Set.map mapping getVars)
  where
    mapping = rename renames

mergeVars :: MonadA m => Map Guid Guid -> RefVars -> RefVars -> InferT def m RefVars
mergeVars renames aRefVars bRefVars
  | any (`Map.notMember` aScope) (Set.toList bGetVars)
  || any (`Map.notMember` bScope) (Set.toList aGetVars)
  = InferT.inferError VarEscapesScope
  | otherwise
  = (`RefVars` Set.union aGetVars bGetVars)
    <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    RefVars aScope aGetVars = aRefVars
    RefVars bScope bGetVars = renameRefVars renames bRefVars
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

mergeBodies ::
  (Eq def, MonadA m) =>
  Map Guid Guid ->
  Expr.Body def Ref ->
  Expr.Body def Ref ->
  InferT def m (Expr.Body def Ref)
mergeBodies _ a (Expr.BodyLeaf Expr.Hole) = return a
mergeBodies renames (Expr.BodyLeaf Expr.Hole) b =
  b & ExprLens.bodyParameterRef %~ rename renames & return
mergeBodies renames a b = do
  case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther matchGetPar a b of
    Nothing -> InferT.inferError $ Mismatch a b
    Just mkBody -> mkBody
  where
    matchLamResult aGuid bGuid aRef bRef = do
      unify (renames & Lens.at bGuid .~ Just aGuid) aRef bRef
    matchOther = unify renames
    matchGetPar aGuid bGuid = aGuid == rename renames bGuid

mergeRefData :: (Eq def, MonadA m) => Map Guid Guid -> RefData def -> RefData def -> InferT def m (RefData def)
mergeRefData renames (RefData aVars aBody) (RefData bVars bBody) =
  RefData
  <$> mergeVars renames aVars bVars
  <*> mergeBodies renames aBody bBody

unify :: (Eq def, MonadA m) => Map Guid Guid -> Ref -> Ref -> InferT def m Ref
unify = ExprRefs.unifyRefs . mergeRefData

-- TODO: Shut ghc up
_unify :: (Eq def, MonadA m) => Map Guid Guid -> Ref -> Ref -> InferT def m Ref
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
  Expr.Expression (LoadedDef def) a ->
  InferT def m (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer (Expr.Expression _body _a) = do
  error "TODO"
