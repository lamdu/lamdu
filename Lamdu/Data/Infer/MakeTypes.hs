module Lamdu.Data.Infer.MakeTypes (makeTypeRef) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (scopeNormalize)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule (verifyTagId)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Internal as Infer
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Apply as RuleApply
import qualified Lamdu.Data.Infer.Rule.GetField as RuleGetField
import qualified Lamdu.Data.Infer.Trigger as Trigger

scopeLookup :: Infer.Scope def -> Guid -> Infer def (ExprRef def)
scopeLookup scope guid = do
  scopeNorm <- InferM.liftGuidAliases $ scopeNormalize scope
  guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
  case scopeNorm ^. Infer.scopeMap . Lens.at guidRep of
    Nothing -> InferM.error VarNotInScope
    Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> Infer.TypedValue def -> Infer.TypedValue def ->
  Expr.Body def (ExprRef def)
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. Infer.tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. Infer.tvType)

makeApplyType ::
  Eq def => Infer.Scope def -> Infer.ScopedTypedValue def -> Infer.ScopedTypedValue def ->
  Infer def (ExprRef def)
makeApplyType applyScope func arg = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. Infer.stvScope)
    (func ^. Infer.stvTV . Infer.tvType)
  void $ unify (arg ^. Infer.stvTV . Infer.tvType) piParamType
  applyTypeRef <- InferM.liftUFExprs $ Infer.freshHole applyScope
  RuleApply.make piGuid (arg ^. Infer.stvTV . Infer.tvVal) piResultRef applyTypeRef
  return applyTypeRef

addTagVerification :: ExprRef def -> Infer def ()
addTagVerification = Trigger.add Trigger.OnDirectlyTag verifyTagId

makeGetFieldType :: Eq def => Infer.Scope def -> Expr.GetField (Infer.TypedValue def) -> Infer def (ExprRef def)
makeGetFieldType scope (Expr.GetField record tag) = do
  tagTypeRef <- InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. Infer.tvType
  getFieldTypeRef <- InferM.liftUFExprs $ Infer.freshHole scope
  addTagVerification $ tag ^. Infer.tvVal
  RuleGetField.make (tag ^. Infer.tvVal) getFieldTypeRef (record ^. Infer.tvType)
  return getFieldTypeRef

makeLambdaType ::
  Eq def => Infer.Scope def -> Guid ->
  Infer.TypedValue def -> Infer.TypedValue def -> Infer def (ExprRef def)
makeLambdaType scope paramGuid paramType result = do
  typeRef <- InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. Infer.tvType
  InferM.liftUFExprs . Infer.fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType ::
  Eq def => Expr.Kind -> Infer.Scope def ->
  [(Infer.TypedValue def, Infer.TypedValue def)] -> Infer def (ExprRef def)
makeRecordType k scope fields = do
  tagTypeRef <- mkFresh $ ExprLens.bodyTagType # ()
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . Infer.tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . Infer.tvVal) addTagVerification
  when (k == Expr.KType) $ do
    typeRef <- mkFresh $ ExprLens.bodyType # ()
    fields & Lens.traverseOf_ (Lens.traverse . Lens._2 . Infer.tvType) (unify typeRef)
  InferM.liftUFExprs . Infer.fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    mkFresh = InferM.liftUFExprs . Infer.fresh scope
    onRecVField (tag, val) = (tag ^. Infer.tvVal, val ^. Infer.tvType)

makePiType ::
  Eq def => Infer.Scope def ->
  Infer.TypedValue def -> Infer.TypedValue def -> Infer def (ExprRef def)
makePiType scope paramType resultType = do
  typeRef <- InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. Infer.tvType
  void . unify typeRef $ resultType ^. Infer.tvType
  return typeRef

makeTypeRef ::
  Eq def => Infer.Scope def ->
  Expr.Body (Infer.LoadedDef def) (Infer.ScopedTypedValue def) ->
  Infer def (ExprRef def)
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType resultType) ->
    makePiType scope (paramType ^. Infer.stvTV) (resultType ^. Infer.stvTV)
  Expr.BodyLeaf Expr.LiteralInteger {} -> InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyIntegerType # ()
  Expr.BodyLeaf Expr.Tag {} -> InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyTagType # ()
  Expr.BodyLeaf Expr.Hole -> InferM.liftUFExprs $ Infer.freshHole scope
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (Infer.LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField -> makeGetFieldType scope ((^. Infer.stvTV) <$> getField)
  Expr.BodyApply (Expr.Apply func arg) -> makeApplyType scope func arg
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    makeLambdaType scope paramGuid (paramType ^. Infer.stvTV) (result ^. Infer.stvTV)
  Expr.BodyRecord (Expr.Record k fields) ->
    makeRecordType k scope $ fields <&> Lens.both %~ (^. Infer.stvTV)
  where
    typeIsType = InferM.liftUFExprs . Infer.fresh scope $ ExprLens.bodyType # ()
