{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( InferT, Context, Error(..)
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  , infer, unify
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Set (Set)
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

data TypedValue = TypedValue
  { _tvVal :: {-# UNPACK #-}! Ref
  , _tvType :: {-# UNPACK #-}! Ref
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

data ScopedTypedValue = ScopedTypedValue
  { _stvTV :: TypedValue
  , _stvScope :: Map Guid Ref
  }
Lens.makeLenses ''ScopedTypedValue

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
      unifyRename (renames & Lens.at bGuid .~ Just aGuid) aRef bRef
    matchOther = unifyRename renames
    matchGetPar aGuid bGuid = aGuid == rename renames bGuid

mergeRefData :: (Eq def, MonadA m) => Map Guid Guid -> RefData def -> RefData def -> InferT def m (RefData def)
mergeRefData renames (RefData aVars aBody) (RefData bVars bBody) =
  RefData
  <$> mergeVars renames aVars bVars
  <*> mergeBodies renames aBody bBody

unifyRename :: (Eq def, MonadA m) => Map Guid Guid -> Ref -> Ref -> InferT def m Ref
unifyRename = ExprRefs.unifyRefs . mergeRefData

unify :: (Eq def, MonadA m) => Ref -> Ref -> InferT def m Ref
unify = unifyRename Map.empty

infer ::
  MonadA m =>
  Map Guid Ref -> Expr.Expression (LoadedDef def) a ->
  InferT def m (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope = exprIntoSTV scope . ExprUtil.annotateUsedVars

-- With hole apply vals and hole types
exprIntoSTV ::
  MonadA m =>
  Map Guid Ref ->
  Expr.Expression (LoadedDef def) (Set Guid, a) ->
  InferT def m
  (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body (usedVars, pl)) = do
  (newBody, typeRef) <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      typeRef <- mkHoleRef
      return
        ( Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
        , typeRef
        )
    Expr.BodyLeaf leaf@(Expr.GetVariable (Expr.ParameterRef guid)) ->
      return
      ( Expr.BodyLeaf leaf
      , unsafeUnjust "GetVar out of scope!" $ scope ^. Lens.at guid
      )
    Expr.BodyLeaf leaf@(Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) ->
      return (Expr.BodyLeaf leaf, ref)
    _ ->
      (,)
      <$> (body & Lens.traverse %%~ exprIntoSTV scope)
      <*> mkHoleRef
  valRef <-
    newBody
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & RefData (RefVars scope usedVars)
    & circumcizeApply
    & ExprRefs.fresh
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    mkHoleRef = ExprRefs.fresh . RefData (RefVars scope Set.empty) $ ExprLens.bodyHole # ()
    circumcizeApply (RefData (RefVars s _) Expr.BodyApply{}) =
      RefData (RefVars s Set.empty) $ ExprLens.bodyHole # ()
    circumcizeApply x = x
