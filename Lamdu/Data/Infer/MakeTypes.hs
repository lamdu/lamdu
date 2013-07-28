module Lamdu.Data.Infer.MakeTypes (makeTypeRef) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.AppliedPiResult (handleAppliedPiResult)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.Rule.Internal (verifyTagId)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule as Rule
import qualified Lamdu.Data.Infer.Trigger as Trigger

scopeLookup :: Scope def -> Guid -> Either (Error def) (RefD def)
scopeLookup scope guid =
  case scope ^. scopeMap . Lens.at guid of
  Nothing -> Left VarNotInScope
  Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> TypedValue def -> TypedValue def ->
  Expr.Body def (RefD def)
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. tvType)

makeApplyType ::
  Eq def => Scope def -> ScopedTypedValue def -> ScopedTypedValue def ->
  Infer def (RefD def)
makeApplyType applyScope func arg = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. stvScope)
    (func ^. stvTV . tvType)
  void $ unify (arg ^. stvTV . tvType) piParamType
  applyTypeRef <- InferM.liftExprRefs $ freshHole applyScope
  handleAppliedPiResult piResultRef AppliedPiResult
    { _aprPiGuid = piGuid
    , _aprArgVal = arg ^. stvTV . tvVal
    , _aprDestRef = applyTypeRef
    , _aprCopiedNames = mempty
    }
  return applyTypeRef

addTagVerification :: RefD def -> Infer def ()
addTagVerification = Trigger.add TriggerIsDirectlyTag verifyTagId

makeGetFieldType :: Eq def => Scope def -> Expr.GetField (TypedValue def) -> Infer def (RefD def)
makeGetFieldType scope (Expr.GetField record tag) = do
  tagTypeRef <- InferM.liftExprRefs . fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. tvType
  getFieldTypeRef <- InferM.liftExprRefs $ freshHole scope
  addTagVerification $ tag ^. tvVal
  Rule.makeGetField (tag ^. tvVal) getFieldTypeRef (record ^. tvType)
  return getFieldTypeRef

makeLambdaType ::
  Eq def => Scope def -> Guid ->
  TypedValue def -> TypedValue def -> Infer def (RefD def)
makeLambdaType scope paramGuid paramType result = do
  typeRef <- InferM.liftExprRefs . fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  InferM.liftExprRefs . fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType ::
  Eq def => Expr.Kind -> Scope def ->
  [(TypedValue def, TypedValue def)] -> Infer def (RefD def)
makeRecordType k scope fields = do
  tagTypeRef <- InferM.liftExprRefs . fresh scope $ ExprLens.bodyTagType # ()
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvVal) addTagVerification
  InferM.liftExprRefs . fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    onRecVField (tag, val) = (tag ^. tvVal, val ^. tvType)

makePiType ::
  Eq def => Scope def ->
  TypedValue def -> TypedValue def -> Infer def (RefD def)
makePiType scope paramType resultType = do
  typeRef <- InferM.liftExprRefs . fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  void . unify typeRef $ resultType ^. tvType
  return typeRef

makeTypeRef ::
  Eq def => Scope def ->
  Expr.Body (LoadedDef def) (ScopedTypedValue def) ->
  Infer def (RefD def)
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType resultType) ->
    makePiType scope (paramType ^. stvTV) (resultType ^. stvTV)
  Expr.BodyLeaf Expr.LiteralInteger {} -> InferM.liftExprRefs . fresh scope $ ExprLens.bodyIntegerType # ()
  Expr.BodyLeaf Expr.Tag {} -> InferM.liftExprRefs . fresh scope $ ExprLens.bodyTagType # ()
  Expr.BodyLeaf Expr.Hole -> InferM.liftExprRefs $ freshHole scope
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
    typeIsType = InferM.liftExprRefs . fresh scope $ ExprLens.bodyType # ()
