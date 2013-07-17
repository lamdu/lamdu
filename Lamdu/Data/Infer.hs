{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  -- Re-export:
  , Context
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

emptyContext :: Context def
emptyContext =
  Context
  { _ctxExprRefs =
    ExprRefs
    { _exprRefsUF = UF.empty
    , _exprRefsData = mempty
    }
  , _ctxDefRefs = Map.empty
  }

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

data Error def
  = VarEscapesScope
  | VarNotInScope
  | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)
  deriving (Show)

type Infer def = StateT (Context def) (Either (Error def))

-- If we don't assert that the scopes have same refs we could be pure
intersectScopes :: Scope -> Scope -> Infer def Scope
intersectScopes (Scope aScope) (Scope bScope) =
  Scope <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

mergeBodies ::
  Eq def =>
  (Map Guid Guid -> Ref -> Maybe Ref -> Infer def Ref) ->
  Map Guid Guid ->
  Expr.Body def Ref ->
  Expr.Body def Ref ->
  Infer def (Expr.Body def Ref)
mergeBodies recurse renames a (Expr.BodyLeaf Expr.Hole) =
  a <$ traverse_ (flip (recurse renames) Nothing) a
mergeBodies _ _ (Expr.BodyLeaf Expr.Hole) b = return b
mergeBodies recurse renames a b = do
  case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther matchGetPar a b of
    Nothing -> lift . Left $ Mismatch a b
    Just mkBody -> mkBody
  where
    matchLamResult aGuid bGuid aRef bRef =
      recurse (renames & Lens.at aGuid .~ Just bGuid) aRef (Just bRef)
    matchOther x y = recurse renames x (Just y)
    matchGetPar aGuid bGuid = aGuid == rename renames bGuid

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & scopeMap %~ Map.mapKeys (rename renames))
    (body & ExprLens.bodyParameterRef %~ rename renames)

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> Maybe Ref -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def (RefData def)
renameMergeRefData recurse renames a b =
  mergeRefData recurse renames (renameRefData renames a) b

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> Maybe Ref -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def (RefData def)
mergeRefData recurse renames (RefData aScope aBody) (RefData bScope bBody) =
  RefData
  <$> intersectScopes aScope bScope
  <*> mergeBodies recurse renames aBody bBody

unifyRename :: Eq def => Set Ref -> Map Guid Guid -> Ref -> Maybe Ref -> Infer def Ref
unifyRename visited renames rawNode mOther = do
  node <- ExprRefs.find "unifyRename:rawNode" rawNode
  if visited ^. Lens.contains node
    then return node
    else
    case mOther of
    Nothing -> do
      nodeData <- ExprRefs.readRep node
      let renamedNodeData = renameRefData renames nodeData
      ExprRefs.writeRep node renamedNodeData
      traverse_ (flip (recurse node renames) Nothing) $
        renamedNodeData ^. rdBody
      return node
    Just other ->
      ExprRefs.unifyRefs merge node other
      <&> fromMaybe node
  where
    recurse visitedRef = unifyRename (visited & Lens.contains visitedRef .~ True)
    merge ref a b =
      renameMergeRefData (recurse ref) renames a b <&> flip (,) ref

unify :: Eq def => Ref -> Ref -> Infer def ()
unify x y = void $ unifyRename mempty mempty x (Just y)

infer ::
  Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer = exprIntoSTV

scopeLookup :: Scope -> Guid -> Either (Error def) Ref
scopeLookup scope guid =
  case scope ^. scopeMap . Lens.at guid of
  Nothing -> Left VarNotInScope
  Just ref -> pure ref

makePiType ::
  Guid -> TypedValue -> TypedValue ->
  Expr.Body def Ref
makePiType paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. tvType)

makeRecordType ::
  [(TypedValue, TypedValue)] ->
  Expr.Body def Ref
makeRecordType fields =
  Expr.BodyRecord . Expr.Record Expr.KType $ onField <$> fields
  where
    onField (tag, val) = (tag ^. tvVal, val ^. tvType)

makeTypeRef ::
  Scope ->
  Expr.Body (LoadedDef def) TypedValue ->
  Infer def Ref
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ _ _) -> typeIsType
  Expr.BodyRecord (Expr.Record Expr.KType _) -> typeIsType
  Expr.BodyLeaf Expr.LiteralInteger {} -> fresh $ ExprLens.bodyIntegerType # ()
  -- Unknown
  Expr.BodyApply {} -> unknownType
  Expr.BodyGetField {} -> unknownType
  Expr.BodyLeaf Expr.Hole -> unknownType
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> lift $ scopeLookup scope guid
  Expr.BodyLeaf Expr.Tag {} -> fresh $ ExprLens.bodyTagType # ()
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) -> fresh $ makePiType paramGuid paramType result
  Expr.BodyRecord (Expr.Record Expr.KVal fields) -> fresh $ makeRecordType fields
  where
    unknownType = fresh $ ExprLens.bodyHole # ()
    fresh = ExprRefs.fresh . RefData scope
    typeIsType = fresh $ ExprLens.bodyType # ()

-- With hole apply vals and hole types
exprIntoSTV ::
  Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  newBody <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & scopeMap . Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
    _ ->
      body & Lens.traverse %%~ exprIntoSTV scope
  valRef <-
    newBody
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & circumcizeApply
    & RefData scope
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1 . stvTV) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    circumcizeApply Expr.BodyApply{} = ExprLens.bodyHole # ()
    circumcizeApply x = x
