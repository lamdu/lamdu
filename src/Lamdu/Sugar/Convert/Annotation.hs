module Lamdu.Sugar.Convert.Annotation
    ( makeAnnotation
    ) where

import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Sugar.Annotations
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeAnnotation ::
    MonadTransaction n m =>
    Annotations.Mode ->
    (ShowAnnotation, EvalPrep) ->
    m (Sugar.Annotation EvalPrep InternalName)
makeAnnotation annMode (showAnn, x) =
    case annMode of
    _ | showAnn ^. showTypeAlways -> pure Sugar.AnnotationNone
    Annotations.Evaluation | showAnn ^. showInEvalMode -> Sugar.AnnotationVal x & pure
    _ -> pure Sugar.AnnotationNone
