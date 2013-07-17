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
import Data.Function (on)
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
import qualified Data.Set as Set
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified System.Random as Random

emptyContext :: Random.StdGen -> Context def
emptyContext gen =
  Context
  { _ctxExprRefs =
    ExprRefs
    { _exprRefsUF = UF.empty
    , _exprRefsData = mempty
    }
  , _ctxDefRefs = Map.empty
  , _ctxRandomGen = gen
  }

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

data Error def
  = VarEscapesScope
  | VarNotInScope
  | InfiniteType Ref
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

newtype HoleConstraints = HoleConstraints
  { hcUnusableInHoleScope :: Set Guid
  }

-- You must apply this recursively
applyHoleConstraints ::
  HoleConstraints -> RefData def -> Either (Error def) (RefData def)
applyHoleConstraints (HoleConstraints unusableSet) (RefData scope substs mRenameHistory body)
  | Lens.anyOf ExprLens.bodyParameterRef (`Set.member` unusableSet) body =
    Left VarEscapesScope
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Set.member` unusableSet) body =
    error "applyHoleConstraints: Shadowing detected"
  | otherwise =
    return $ RefData newScope substs mRenameHistory body
  where
    unusableMap = Map.fromSet (const ()) unusableSet
    newScope = scope & scopeMap %~ (`Map.difference` unusableMap)

data UnifyPhase
  = UnifyHoleConstraints HoleConstraints
  | UnifyRef Ref

mergeBodies ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid ->
  RefData def -> RefData def ->
  Infer def (Expr.Body def Ref)
mergeBodies recurse renames x y =
  case (x ^. rdBody, y ^. rdBody) of
  (xBody, Expr.BodyLeaf Expr.Hole) ->
    xBody <$ traverse_
    (flip (recurse renames) (unifyHoleConstraints y x)) xBody
  (Expr.BodyLeaf Expr.Hole, yBody) ->
    yBody <$ traverse_
    (flip (recurse Map.empty) (unifyHoleConstraints x y)) yBody
  (xBody, yBody) ->
    case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther (==) xBody yBody of
    Nothing -> lift . Left $ Mismatch xBody yBody
    Just mkBody -> mkBody
  where
    unifyHoleConstraints hole other =
      UnifyHoleConstraints HoleConstraints
      { hcUnusableInHoleScope =
        Map.keysSet $ Map.difference
        (other ^. rdScope . scopeMap)
        (hole ^. rdScope . scopeMap)
      }
    matchLamResult xGuid yGuid xRef yRef =
      recurse (renames & Lens.at xGuid .~ Just yGuid) xRef (UnifyRef yRef)
    matchOther xRef yRef = recurse renames xRef (UnifyRef yRef)

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope substs mRenameHistory body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & scopeMap %~ Map.mapKeys (rename renames))
    {-TODO:-}substs
    (mRenameHistory <&> Map.union renames)
    (body & ExprLens.bodyParameterRef %~ rename renames)

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def (RefData def)
mergeRefData recurse renames a b =
  mkRefData
  <$> intersectScopes (a ^. rdScope) (b ^. rdScope)
  <*> mergeBodies recurse renames a b
  where
    mkRefData scope body =
      RefData
      { _rdScope = scope
      , _rdSubsts = on (++) (^. rdSubsts) a b
      , _rdMRenameHistory = Nothing
      , _rdBody = body
      }

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def (RefData def)
renameMergeRefData recurse renames a b =
  mergeRefData recurse renames (renameRefData renames a) b

unifyRecurse :: Eq def => Set Ref -> Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref
unifyRecurse visited renames rawNode phase = do
  nodeRep <- ExprRefs.find "unifyRecurse:rawNode" rawNode
  if visited ^. Lens.contains nodeRep
    then lift . Left $ InfiniteType nodeRep
    else
    case phase of
    UnifyHoleConstraints holeConstraints -> do
      rawNodeData <- ExprRefs.readRep nodeRep
      nodeData <-
        lift . applyHoleConstraints holeConstraints $
        renameRefData renames rawNodeData
      ExprRefs.writeRep nodeRep nodeData
      traverse_
        (flip (recurse nodeRep renames)
         (UnifyHoleConstraints holeConstraints)) $
        nodeData ^. rdBody
      return nodeRep
    UnifyRef other ->
      ExprRefs.unifyRefs merge nodeRep other
      <&> fromMaybe nodeRep
  where
    recurse visitedRef = unifyRecurse (visited & Lens.contains visitedRef .~ True)
    merge ref a b =
      renameMergeRefData (recurse ref) renames a b <&> flip (,) ref

unify :: Eq def => Ref -> Ref -> Infer def ()
unify x y = void $ unifyRecurse mempty mempty x (UnifyRef y)

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
  Expr.BodyLeaf Expr.Tag {} -> fresh $ ExprLens.bodyTagType # ()
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> lift $ scopeLookup scope guid
  -- Unknown
  Expr.BodyGetField {} -> unknownType -- TODO
  Expr.BodyLeaf Expr.Hole -> unknownType
  -- Other:
  Expr.BodyApply {} -> unknownType
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) -> fresh $ makePiType paramGuid paramType result
  Expr.BodyRecord (Expr.Record Expr.KVal fields) -> fresh $ makeRecordType fields
  where
    unknownType = fresh $ ExprLens.bodyHole # ()
    fresh typeBody = ExprRefs.fresh RefData
      { _rdScope = scope
      , _rdSubsts = []
      , _rdMRenameHistory = Nothing
      , _rdBody = typeBody
      }
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
    & RefData scope {- TODO: -}[] Nothing
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1 . stvTV) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    circumcizeApply Expr.BodyApply{} = ExprLens.bodyHole # ()
    circumcizeApply x = x
