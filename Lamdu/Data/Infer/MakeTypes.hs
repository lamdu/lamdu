module Lamdu.Data.Infer.MakeTypes (makeTV) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (Scope, scopeNormalizeParamRefs)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule (verifyTagId)
import Lamdu.Data.Infer.TypedValue (TypedValue(..), tvVal, tvType)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Load as Load
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Rule.Apply as RuleApply
import qualified Lamdu.Data.Infer.Rule.GetField as RuleGetField
import qualified Lamdu.Data.Infer.Rule.Uncircumsize as RuleUncircumsize
import qualified Lamdu.Data.Infer.Trigger as Trigger

scopeLookup :: Scope def -> Guid -> Infer def (ExprRef def)
scopeLookup scope guid = do
  scopeNorm <- InferM.liftGuidAliases $ scopeNormalizeParamRefs scope
  guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
  case scopeNorm ^. RefData.scopeMap . Lens.at guidRep of
    Nothing -> InferM.error VarNotInScope
    Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> TypedValue def -> TypedValue def ->
  Expr.Body (Load.LoadedDef def) (ExprRef def)
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. tvType)

fresh ::
  Ord def =>
  Scope def -> Expr.Body (Load.LoadedDef def) (ExprRef def) ->
  Infer def (ExprRef def)
fresh scope body = InferM.liftContext $ Context.fresh scope body

maybeCircumsize ::
  Ord def =>
  Scope def ->
  TypedValue def ->
  Expr.Body (Load.LoadedDef def) (TypedValue def) ->
  ExprRef def ->
  Infer def (TypedValue def)
maybeCircumsize scope applicant uncircumsizedValBody typeRef = do
  -- We only maybeCircumsize non-tags:
  valRef <-
    RefData.defaultRefData scope (ExprLens.bodyHole # ())
    & RefData.rdWasNotDirectlyTag .~ Monoid.Any True
    & Context.freshData
    & InferM.liftContext
  RuleUncircumsize.make valRef
    (applicant ^. tvVal)
    (uncircumsizedValBody <&> (^. tvVal))
  return $ TypedValue valRef typeRef

makeApplyTV ::
  Ord def => Scope def -> Expr.Apply (TypedValue def) ->
  Infer def (TypedValue def)
makeApplyTV applyScope apply@(Expr.Apply func arg) = do
  funcScope <-
    UFData.read (func ^. tvType)
    & InferM.liftUFExprs
    <&> (^. RefData.rdScope)
  (piGuid, piParamType, piResultRef) <- forceLam Expr.KType funcScope $ func ^. tvType
  void $ unify (arg ^. tvType) piParamType
  applyTypeRef <- InferM.liftContext $ Context.freshHole applyScope
  RuleApply.make piGuid (arg ^. tvVal) piResultRef applyTypeRef
  maybeCircumsize applyScope func (Expr.BodyApply apply) applyTypeRef

addTagVerification :: ExprRef def -> Infer def ()
addTagVerification = Trigger.add [RefData.MustBeTag] Trigger.OnDirectlyTag verifyTagId

makeGetFieldTV ::
  Ord def =>
  Scope def -> Expr.GetField (TypedValue def) -> Infer def (TypedValue def)
makeGetFieldTV scope getField@(Expr.GetField record tag) = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. tvType
  getFieldTypeRef <- InferM.liftContext $ Context.freshHole scope
  addTagVerification $ tag ^. tvVal
  RuleGetField.make (tag ^. tvVal) getFieldTypeRef (record ^. tvType)
  maybeCircumsize scope record (Expr.BodyGetField getField) getFieldTypeRef

makeLambdaType ::
  Ord def => Scope def -> Guid ->
  TypedValue def -> TypedValue def -> Infer def (ExprRef def)
makeLambdaType scope paramGuid paramType result = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType ::
  Ord def => Expr.Kind -> Scope def ->
  [(TypedValue def, TypedValue def)] -> Infer def (ExprRef def)
makeRecordType k scope fields = do
  tagTypeRef <- mkFresh $ ExprLens.bodyTagType # ()
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvVal) addTagVerification
  when (k == Expr.KType) $ do
    typeRef <- mkFresh $ ExprLens.bodyType # ()
    fields & Lens.traverseOf_ (Lens.traverse . Lens._2 . tvType) (unify typeRef)
  fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    mkFresh = fresh scope
    onRecVField (tag, val) = (tag ^. tvVal, val ^. tvType)

makePiType ::
  Ord def => Scope def ->
  TypedValue def -> TypedValue def ->
  Infer def (ExprRef def)
makePiType scope paramType resultType = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  void . unify typeRef $ resultType ^. tvType
  return typeRef

makeTV ::
  Ord def => Scope def ->
  Expr.Body (Load.LoadedDef def) (TypedValue def) ->
  Infer def (TypedValue def)
makeTV scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLeaf Expr.LiteralInteger {} ->
    uncircumsized <*> freshBody (ExprLens.bodyIntegerType # ())
  Expr.BodyLeaf Expr.Tag {} ->
    uncircumsized <*> freshBody (ExprLens.bodyTagType # ())
  Expr.BodyLeaf Expr.Hole -> do
    valRef <- freshVal
    typRef <- InferM.liftContext $ Context.freshHole scope
    return $ TypedValue valRef typRef
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (Load.LoadedDef _ ref))) ->
    uncircumsized <*> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) ->
    uncircumsized <*> scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField -> makeGetFieldTV scope getField
  Expr.BodyApply apply -> makeApplyTV scope apply
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType result) ->
    uncircumsized <*> makePiType scope paramType result
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    uncircumsized <*> makeLambdaType scope paramGuid paramType result
  Expr.BodyRecord (Expr.Record k fields) ->
    uncircumsized <*> makeRecordType k scope fields
  where
    freshBody = fresh scope
    freshVal = fresh scope (body <&> (^. tvVal))
    uncircumsized = TypedValue <$> freshVal
    mkRefWithType = freshBody $ ExprLens.bodyType # ()
    typeIsType = uncircumsized <*> mkRefWithType
