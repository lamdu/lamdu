{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Expression.Infer.Unify
  ( InferT, Context, Error(..), TypedValue(..), ScopedTypedValue(..)
  , infer
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Expression.Infer.Unify.Internal
import Lamdu.Data.Expression.Infer.Unify.Monad (InferT)
import Lamdu.Data.Expression.Infer.UnionFind (Ref)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer.Unify.ExprRefs as ExprRefs
import qualified Lamdu.Data.Expression.Infer.Unify.Monad as InferT
import qualified Lamdu.Data.Expression.Infer.UnionFind as UF
import qualified Lamdu.Data.Expression.Utils as ExprUtil

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
      equiv <- ExprRefs.exprRefsEquiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

sameParameter :: Monad m => Guid -> Guid -> InferT def m Bool
sameParameter xGuid yGuid = InferT.liftContext . Lens.zoom ctxGuidAliases $ do
  guidMap <- Lens.use gaMap
  let
    xRef = unsafeUnjust "sameParameter:x" $ guidMap ^. Lens.at xGuid
    yRef = unsafeUnjust "sameParameter:y" $ guidMap ^. Lens.at yGuid
  Lens.zoom gaUF $ UF.equivalent xRef yRef

unifyGuids :: Monad m => Guid -> Guid -> InferT def m ()
unifyGuids aGuid bGuid = InferT.liftContext . Lens.zoom ctxGuidAliases $ do
  setDefault aGuid
  setDefault bGuid
  where
    setDefault guid = do
      ref <-
        maybe (Lens.zoom gaUF UF.freshRef) return =<<
        Lens.use (gaMap . Lens.at guid)
      gaMap . Lens.at guid .= Just ref

mergeBodies ::
  (Eq def, MonadA m) =>
  Expr.Body def Ref ->
  Expr.Body def Ref ->
  InferT def m (Expr.Body def Ref)
mergeBodies a (Expr.BodyLeaf Expr.Hole) = return a
mergeBodies (Expr.BodyLeaf Expr.Hole) b = return b
mergeBodies a b = do
  mBody <- ExprUtil.matchBodyG matchLamResult matchOther matchGetPar a b
  case mBody of
    Nothing -> InferT.inferError $ Mismatch a b
    Just body -> return body
  where
    matchLamResult aGuid bGuid aRef bRef = do
      unifyGuids aGuid bGuid
      unifyRefs aRef bRef
    matchOther = unifyRefs
    matchGetPar = sameParameter

mergeRefData :: (Eq def, MonadA m) => RefData def -> RefData def -> InferT def m (RefData def)
mergeRefData (RefData aVars aBody) (RefData bVars bBody) =
  RefData <$> mergeVars aVars bVars <*> mergeBodies aBody bBody

unifyRefs :: (Eq def, MonadA m) => Ref -> Ref -> InferT def m Ref
unifyRefs x y = do
  xRep <- ExprRefs.exprRefsFind "unify.x" x
  yRep <- ExprRefs.exprRefsFind "unify.y" y
  if xRep == yRep
    then return xRep
    else do
      xData <- ExprRefs.exprRefsPopRep xRep
      yData <- ExprRefs.exprRefsPopRep yRep
      rep <- ExprRefs.exprRefsUnion x y
      ExprRefs.exprRefsWriteRep rep $ error "Attempt to read parent during unification"
      newData <- mergeRefData xData yData
      ExprRefs.exprRefsWriteRep rep newData
      return rep

-- TODO: Shut warnings up for now:
_unifyRefs :: (Eq def, MonadA m) => Ref -> Ref -> InferT def m Ref
_unifyRefs = unifyRefs

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
