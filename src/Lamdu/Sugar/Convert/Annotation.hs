module Lamdu.Sugar.Convert.Annotation
    ( makeAnnotation, makeTypeAnnotation
    ) where

import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Annotations
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderType)
import           Lamdu.Sugar.Convert.Type (convertType)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeAnnotation ::
    MonadTransaction n m =>
    Annotations.Mode ->
    (ShowAnnotation, EvalPrep) ->
    m (Sugar.Annotation EvalPrep InternalName)
makeAnnotation annMode (showAnn, x) =
    case annMode of
    _ | showAnn ^. showTypeAlways -> typeAnnotationFromEvalRes x
    Annotations.Types | showAnn ^. showInTypeMode -> typeAnnotationFromEvalRes x
    Annotations.Evaluation | showAnn ^. showInEvalMode -> Sugar.AnnotationVal x & pure
    _ -> pure Sugar.AnnotationNone

typeAnnotationFromEvalRes ::
    MonadTransaction n f => EvalPrep -> f (Sugar.Annotation v InternalName)
typeAnnotationFromEvalRes x =
    makeTypeAnnotation (x ^. eEvalId) (x ^. eType) <&> Sugar.AnnotationType

makeTypeAnnotation ::
    MonadTransaction n m =>
    EntityId -> Pure # T.Type -> m (Annotated EntityId # Sugar.Type InternalName)
makeTypeAnnotation e t = convertType (EntityId.ofTypeOf e) t >>= orderType
