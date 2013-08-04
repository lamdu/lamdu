module Lamdu.Data.Infer.MakeTypes (makeTV) where

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
import qualified Data.Monoid as Monoid
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Internal as Infer
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Apply as RuleApply
import qualified Lamdu.Data.Infer.Rule.Uncircumsize as RuleUncircumsize
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
  Expr.Body (Infer.LoadedDef def) (ExprRef def)
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. Infer.tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. Infer.tvType)

fresh ::
  Infer.Scope def -> Expr.Body (Infer.LoadedDef def) (ExprRef def) ->
  Infer def (ExprRef def)
fresh scope body =
  InferM.liftUFExprs $ Infer.fresh scope (body & ExprLens.bodyDef %~ (^. Infer.ldDef))

freshHole :: Infer.Scope def -> Infer def (ExprRef def)
freshHole = InferM.liftUFExprs . Infer.freshHole

maybeCircumsize ::
  Infer.Scope def ->
  Infer.TypedValue def ->
  Expr.Body (Infer.LoadedDef def) (Infer.TypedValue def) ->
  ExprRef def ->
  Infer def (Infer.TypedValue def)
maybeCircumsize scope applicant uncircumsizedValBody typeRef = do
  -- We only maybeCircumsize non-tags:
  valRef <-
    Infer.defaultRefData scope (ExprLens.bodyHole # ())
    & Infer.rdWasNotDirectlyTag .~ Monoid.Any True
    & InferM.liftUFExprs . UFData.fresh
  RuleUncircumsize.make valRef
    (applicant ^. Infer.tvVal)
    ( uncircumsizedValBody
      & ExprLens.bodyDef %~ (^. Infer.ldDef)
      <&> (^. Infer.tvVal)
    )
  return $ Infer.TypedValue valRef typeRef

makeApplyTV ::
  Eq def => Infer.Scope def -> Expr.Apply (Infer.ScopedTypedValue def) ->
  Infer def (Infer.TypedValue def)
makeApplyTV scope apply@(Expr.Apply func arg) = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. Infer.stvScope)
    (func ^. Infer.stvTV . Infer.tvType)
  void $ unify (arg ^. Infer.stvTV . Infer.tvType) piParamType
  applyTypeRef <- freshHole scope
  RuleApply.make piGuid (arg ^. Infer.stvTV . Infer.tvVal) piResultRef applyTypeRef
  maybeCircumsize
    scope
    (func ^. Infer.stvTV)
    (Expr.BodyApply (apply <&> (^. Infer.stvTV)))
    applyTypeRef

addTagVerification :: ExprRef def -> Infer def ()
addTagVerification = Trigger.add Trigger.OnDirectlyTag verifyTagId

makeGetFieldTV ::
  Eq def =>
  Infer.Scope def -> Expr.GetField (Infer.TypedValue def) -> Infer def (Infer.TypedValue def)
makeGetFieldTV scope getField@(Expr.GetField record tag) = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. Infer.tvType
  getFieldTypeRef <- freshHole scope
  addTagVerification $ tag ^. Infer.tvVal
  RuleGetField.make (tag ^. Infer.tvVal) getFieldTypeRef (record ^. Infer.tvType)
  maybeCircumsize scope record (Expr.BodyGetField getField) getFieldTypeRef

makeLambdaType ::
  Eq def => Infer.Scope def -> Guid ->
  Infer.TypedValue def -> Infer.TypedValue def -> Infer def (ExprRef def)
makeLambdaType scope paramGuid paramType result = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. Infer.tvType
  fresh scope $ makePiTypeOfLam paramGuid paramType result

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
  fresh scope $
    case k of
    Expr.KVal -> Expr.BodyRecord . Expr.Record Expr.KType $ onRecVField <$> fields
    Expr.KType -> ExprLens.bodyType # ()
  where
    mkFresh = fresh scope
    onRecVField (tag, val) = (tag ^. Infer.tvVal, val ^. Infer.tvType)

makePiType ::
  Eq def => Infer.Scope def ->
  Infer.TypedValue def -> Infer.TypedValue def ->
  Infer def (ExprRef def)
makePiType scope paramType resultType = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. Infer.tvType
  void . unify typeRef $ resultType ^. Infer.tvType
  return typeRef

makeTV ::
  Eq def => Infer.Scope def ->
  Expr.Body (Infer.LoadedDef def) (Infer.ScopedTypedValue def) ->
  Infer def (Infer.TypedValue def)
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
  Expr.BodyLeaf Expr.Hole ->
    uncircumsized <*> freshBody (ExprLens.bodyHole # ())
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (Infer.LoadedDef _ ref))) ->
    uncircumsized <*> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) ->
    uncircumsized <*> scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField ->
    makeGetFieldTV scope ((^. Infer.stvTV) <$> getField)
  Expr.BodyApply apply -> makeApplyTV scope apply
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType result) ->
    uncircumsized
    <*> makePiType scope
        (paramType ^. Infer.stvTV)
        (result ^. Infer.stvTV)
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    uncircumsized
    <*> makeLambdaType scope
        paramGuid
        (paramType ^. Infer.stvTV)
        (result ^. Infer.stvTV)
  Expr.BodyRecord (Expr.Record k fields) ->
    uncircumsized <*> makeRecordType k scope (fields <&> Lens.both %~ (^. Infer.stvTV))
  where
    freshBody = fresh scope
    uncircumsized =
      Infer.TypedValue <$> fresh scope (body <&> (^. Infer.stvTV . Infer.tvVal))
    mkRefWithType = freshBody $ ExprLens.bodyType # ()
    typeIsType = uncircumsized <*> mkRefWithType
