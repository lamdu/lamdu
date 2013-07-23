module Lamdu.Data.Infer.GetField
  ( handleGetField
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Unify as Unify

data TagBodyMatch = TagBodyMatch | TagBodyMismatch | TagBodyMaybeMatch
  deriving (Eq)

tagBodiesMayMatch :: Expr.Body def a -> Expr.Body def a -> TagBodyMatch
tagBodiesMayMatch (Expr.BodyLeaf Expr.Hole) _ = TagBodyMaybeMatch
tagBodiesMayMatch _ (Expr.BodyLeaf Expr.Hole) = TagBodyMaybeMatch
tagBodiesMayMatch (Expr.BodyLeaf (Expr.Tag a)) (Expr.BodyLeaf (Expr.Tag b)) | a == b = TagBodyMatch
tagBodiesMayMatch _ _ = TagBodyMismatch

handleGetField :: Eq def => GetFieldRefs -> Infer def ()
handleGetField (GetFieldRefs getFieldTagRef getFieldTypeRef recordTypeRef) = do
  recordTypeBody <- InferM.liftContext $ (^. rdBody) <$> ExprRefs.read recordTypeRef
  getFieldTagBody <- InferM.liftContext $ (^. rdBody) <$> ExprRefs.read getFieldTagRef
  case recordTypeBody of
    Expr.BodyLeaf Expr.Hole -> return ()
    Expr.BodyRecord (Expr.Record Expr.KType fields) -> do
      matchedFields <-
        InferM.liftContext $
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
      void $ Unify.unify fieldTagRef getFieldTagRef
      void $ Unify.unify fieldTypeRef getFieldTypeRef
