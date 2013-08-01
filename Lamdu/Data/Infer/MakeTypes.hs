module Lamdu.Data.Infer.MakeTypes (makeTypeRef) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (scopeNormalize)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal (verifyTagId)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule as Rule
import qualified Lamdu.Data.Infer.Trigger as Trigger

scopeLookup :: Scope def -> Guid -> Infer def (ExprRef def)
scopeLookup scope guid = do
  scopeNorm <- InferM.liftGuidAliases $ scopeNormalize scope
  guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
  case scopeNorm ^. scopeMap . Lens.at guidRep of
    Nothing -> InferM.error VarNotInScope
    Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> TypedValue def -> TypedValue def ->
  Expr.Body def (ExprRef def)
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
  Infer def (ExprRef def)
makeApplyType applyScope func arg = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. stvScope)
    (func ^. stvTV . tvType)
  void $ unify (arg ^. stvTV . tvType) piParamType
  applyTypeRef <- InferM.liftUFExprs $ freshHole applyScope
  Rule.makeApply piGuid (arg ^. stvTV . tvVal) piResultRef applyTypeRef
  return applyTypeRef

addTagVerification :: ExprRef def -> Infer def ()
addTagVerification = Trigger.add Trigger.IsDirectlyTag verifyTagId

makeGetFieldType :: Eq def => Scope def -> Expr.GetField (TypedValue def) -> Infer def (ExprRef def)
makeGetFieldType scope (Expr.GetField record tag) = do
  tagTypeRef <- InferM.liftUFExprs . fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. tvType
  getFieldTypeRef <- InferM.liftUFExprs $ freshHole scope
  addTagVerification $ tag ^. tvVal
  Rule.makeGetField (tag ^. tvVal) getFieldTypeRef (record ^. tvType)
  return getFieldTypeRef

makeLambdaType ::
  Eq def => Scope def -> Guid ->
  TypedValue def -> TypedValue def -> Infer def (ExprRef def)
makeLambdaType scope paramGuid paramType result = do
  typeRef <- InferM.liftUFExprs . fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  InferM.liftUFExprs . fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType ::
  Eq def => Expr.Kind -> Scope def ->
  [(TypedValue def, TypedValue def)] -> Infer def (ExprRef def)
makeRecordType k scope fields = do
  tagTypeRef <- mkFresh $ ExprLens.bodyTagType # ()
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvVal) addTagVerification
  when (k == Expr.KType) $ do
    typeRef <- mkFresh $ ExprLens.bodyType # ()
    fields & Lens.traverseOf_ (Lens.traverse . Lens._2 . tvType) (unify typeRef)
  InferM.liftUFExprs . fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    mkFresh = InferM.liftUFExprs . fresh scope
    onRecVField (tag, val) = (tag ^. tvVal, val ^. tvType)

makePiType ::
  Eq def => Scope def ->
  TypedValue def -> TypedValue def -> Infer def (ExprRef def)
makePiType scope paramType resultType = do
  typeRef <- InferM.liftUFExprs . fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  void . unify typeRef $ resultType ^. tvType
  return typeRef

makeTypeRef ::
  Eq def => Scope def ->
  Expr.Body (LoadedDef def) (ScopedTypedValue def) ->
  Infer def (ExprRef def)
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType resultType) ->
    makePiType scope (paramType ^. stvTV) (resultType ^. stvTV)
  Expr.BodyLeaf Expr.LiteralInteger {} -> InferM.liftUFExprs . fresh scope $ ExprLens.bodyIntegerType # ()
  Expr.BodyLeaf Expr.Tag {} -> InferM.liftUFExprs . fresh scope $ ExprLens.bodyTagType # ()
  Expr.BodyLeaf Expr.Hole -> InferM.liftUFExprs $ freshHole scope
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField -> makeGetFieldType scope ((^. stvTV) <$> getField)
  Expr.BodyApply (Expr.Apply func arg) -> makeApplyType scope func arg
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    makeLambdaType scope paramGuid (paramType ^. stvTV) (result ^. stvTV)
  Expr.BodyRecord (Expr.Record k fields) ->
    makeRecordType k scope $ fields <&> Lens.both %~ (^. stvTV)
  where
    typeIsType = InferM.liftUFExprs . fresh scope $ ExprLens.bodyType # ()
