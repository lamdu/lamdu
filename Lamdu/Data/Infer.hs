module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify, tempUnify
  , emptyContext
  -- Re-export:
  , Context
  , Scope, emptyScope
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (StateT)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.Subst (substOrUnify)
import Lamdu.Data.Infer.Unify (unify, forceLam, fresh)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

-- | Temporarily here for tests until we fix API to only have
-- TypedValue unify:
tempUnify :: Eq def => Ref -> Ref -> StateT (Context def) (Either (Error def)) Ref
tempUnify x y = unify x y & runInfer

infer ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope expr = exprIntoSTV scope expr & runInfer

runInfer :: Eq def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer = InferM.run $ InferM.InferActions substOrUnify

scopeLookup :: Scope -> Guid -> Either (Error def) Ref
scopeLookup scope guid =
  case scope ^. scopeMap . Lens.at guid of
  Nothing -> Left VarNotInScope
  Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> TypedValue -> TypedValue ->
  Expr.Body def Ref
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. tvType)

makeApplyType ::
  Eq def => Scope -> ScopedTypedValue -> ScopedTypedValue ->
  Infer def Ref
makeApplyType applyScope func arg = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. stvScope)
    (func ^. stvTV . tvType)
  void $ unify (arg ^. stvTV . tvType) piParamType
  applyTypeRef <- fresh applyScope $ ExprLens.bodyHole # ()
  substOrUnify piResultRef AppliedPiResult
    { _aprPiGuid = piGuid
    , _aprArgVal = arg ^. stvTV . tvVal
    , _aprDestRef = applyTypeRef
    , _aprCopiedNames = mempty
    }
  return applyTypeRef

makeGetFieldType :: Eq def => Scope -> Expr.GetField TypedValue -> Infer def Ref
makeGetFieldType scope (Expr.GetField _record tag) = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. tvType
  fresh scope $ ExprLens.bodyHole # () -- TODO

makeLambdaType :: Eq def => Scope -> Guid -> TypedValue -> TypedValue -> Infer def Ref
makeLambdaType scope paramGuid paramType result = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType :: Eq def => Expr.Kind -> Scope -> [(TypedValue, TypedValue)] -> Infer def Ref
makeRecordType k scope fields = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvVal) setTagPos
  fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    setTagPos ref =
      ExprRefs.modify ref $ rdRelations <>~ Set.singleton RelationIsTag
    onRecVField (tag, val) = (tag ^. tvVal, val ^. tvType)

makePiType :: Eq def => Scope -> TypedValue -> TypedValue -> Infer def Ref
makePiType scope paramType resultType = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  void . unify typeRef $ resultType ^. tvType
  return typeRef

makeTypeRef ::
  Eq def => Scope ->
  Expr.Body (LoadedDef def) ScopedTypedValue ->
  Infer def Ref
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType resultType) ->
    makePiType scope (paramType ^. stvTV) (resultType ^. stvTV)
  Expr.BodyLeaf Expr.LiteralInteger {} -> fresh scope $ ExprLens.bodyIntegerType # ()
  Expr.BodyLeaf Expr.Tag {} -> fresh scope $ ExprLens.bodyTagType # ()
  Expr.BodyLeaf Expr.Hole -> fresh scope $ ExprLens.bodyHole # ()
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) ->
    InferM.liftError $ scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField -> makeGetFieldType scope ((^. stvTV) <$> getField)
  Expr.BodyApply (Expr.Apply func arg) -> makeApplyType scope func arg
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    makeLambdaType scope paramGuid (paramType ^. stvTV) (result ^. stvTV)
  Expr.BodyRecord (Expr.Record k fields) ->
    makeRecordType k scope $ fields <&> Lens.both %~ (^. stvTV)
  where
    typeIsType = fresh scope $ ExprLens.bodyType # ()

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
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
    & circumcizeApply
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & defaultRefData scope
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    -- Except of apply of type constructors:
    circumcizeApply x
      | Lens.nullOf Expr._BodyApply x = x
      | Lens.has (Expr._BodyApply . Expr.applyFunc . ExprLens.exprDefinitionRef) x = x
      | otherwise = ExprLens.bodyHole # ()
