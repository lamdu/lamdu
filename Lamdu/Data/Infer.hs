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
import Data.Foldable (traverse_)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.AppliedPiResult (handleAppliedPiResult)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.MakeTypes (makeTypeRef)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

-- | Temporarily here for tests until we fix API to only have
-- TypedValue unify:
tempUnify :: Eq def => Ref -> Ref -> StateT (Context def) (Either (Error def)) ()
tempUnify x y = unify x y & runInfer & void

infer ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope expr = exprIntoSTV scope expr & runInfer

isTag :: Ref -> Infer def ()
isTag ref = do
  refData <- ExprRefs.read ref
  case refData ^. rdIsComposite of
    Monoid.Any True -> InferM.error $ CompositeTag ref
    _ -> return ()

data TagBodyMatch = TagBodyMatch | TagBodyMismatch | TagBodyMaybeMatch
  deriving (Eq)

tagBodiesMayMatch :: Expr.Body def a -> Expr.Body def a -> TagBodyMatch
tagBodiesMayMatch (Expr.BodyLeaf Expr.Hole) _ = TagBodyMaybeMatch
tagBodiesMayMatch _ (Expr.BodyLeaf Expr.Hole) = TagBodyMaybeMatch
tagBodiesMayMatch (Expr.BodyLeaf (Expr.Tag a)) (Expr.BodyLeaf (Expr.Tag b)) | a == b = TagBodyMatch
tagBodiesMayMatch _ _ = TagBodyMismatch

handleGetField :: Eq def => GetFieldRefs -> Infer def ()
handleGetField (GetFieldRefs getFieldTagRef getFieldTypeRef recordTypeRef) = do
  recordTypeBody <- (^. rdBody) <$> ExprRefs.read recordTypeRef
  getFieldTagBody <- (^. rdBody) <$> ExprRefs.read getFieldTagRef
  case recordTypeBody of
    Expr.BodyLeaf Expr.Hole -> return ()
    Expr.BodyRecord (Expr.Record Expr.KType fields) -> do
      matchedFields <-
        fields
        & Lens.traverse . Lens._1 %%~ \tagRef ->
          ExprRefs.read tagRef
          <&> tagBodiesMayMatch getFieldTagBody . (^. rdBody)
          <&> (,) tagRef
      let ofMatchType t = filter ((== t) . snd . fst) matchedFields <&> Lens._1 %~ fst
      case ofMatchType TagBodyMatch of
        (fieldTagRef, fieldTypeRef) : _ -> void $ unionField fieldTagRef fieldTypeRef
        [] -> -- No exact matches
          case ofMatchType TagBodyMaybeMatch of
          -- This is as good as an exact match:
          [(fieldTagRef, fieldTypeRef)] -> void $ unionField fieldTagRef fieldTypeRef
          [] -> InferM.error GetMissingField
          _ -> return ()
    _ -> InferM.error GetFieldRequiresRecord
  where
    unionField fieldTagRef fieldTypeRef = do
      void $ unify fieldTagRef getFieldTagRef
      void $ unify fieldTypeRef getFieldTypeRef

executeRelation :: Eq def => Relation -> Ref -> Infer def ()
executeRelation rel =
  case rel of
  RelationAppliedPiResult apr -> flip handleAppliedPiResult apr
  RelationGetField getField -> const (handleGetField getField)
  RelationIsTag -> isTag

rerunRelations :: Eq def => Ref -> Infer def ()
rerunRelations ref = do
  relations <- (^. rdRelations) <$> ExprRefs.read ref
  traverse_ (`executeRelation` ref) relations

runInfer :: Eq def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer = InferM.run $ InferM.InferActions rerunRelations

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  bodySTV <-
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
    bodySTV
    & ExprLens.bodyDef %~ (^. ldDef)
    & mkRefData
    & ExprRefs.fresh
  typeRef <-
    bodySTV <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression bodySTV
    (ScopedTypedValue (TypedValue valRef typeRef) scope, pl)
  where
    mkRefData bodySTV
      | shouldCircumsize bodySTV =
        defaultRefData scope (ExprLens.bodyHole # ())
        & rdIsComposite .~ Monoid.Any True
      | otherwise = defaultRefData scope $ bodySTV <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    shouldCircumsize (Expr.BodyApply (Expr.Apply func _))
      | Lens.nullOf ExprLens.exprDefinitionRef func = True
    shouldCircumsize (Expr.BodyGetField (Expr.GetField record _))
      | Lens.nullOf ExprLens.exprDefinitionRef record = True
    shouldCircumsize _ = False
