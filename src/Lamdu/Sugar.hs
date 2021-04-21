-- | Top-level wrapper for Sugar.Convert, Sugar.Parens, Sugar.AddNames

module Lamdu.Sugar
    ( sugarWorkArea
    , Sugar.WorkArea, Sugar.Payload, Sugar.ParenInfo, Sugar.EntityId, Name
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Control.Monad.Transaction (MonadTransaction)
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Data.Tuple (swap)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Tag (Tag, IsOperator, TextsInLang)
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Annotations
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Lamdu.Sugar.Convert as SugarConvert
import           Lamdu.Sugar.Convert.Expression.Actions (makeTypeAnnotation)
import           Lamdu.Sugar.Eval (addEvaluationResults)
import           Lamdu.Sugar.Internal (EvalPrep, eEvalId, eType, eLambdas)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

markAnnotations ::
    Functor i =>
    SugarConfig.Config ->
    Sugar.WorkArea v n i o (Sugar.Payload v n i o, a) ->
    Sugar.WorkArea (ShowAnnotation, v) n i o (Sugar.Payload (ShowAnnotation, v) n i o, a)
markAnnotations config workArea
    | config ^. SugarConfig.showAllAnnotations =
        workArea & SugarLens.workAreaAnnotations %~ (,) alwaysShowAnnotations
    | otherwise =
        workArea
        { Sugar._waPanes = workArea ^. Sugar.waPanes <&> SugarLens.paneBinder %~ markNodeAnnotations
        , Sugar._waRepl = workArea ^. Sugar.waRepl & Sugar.replExpr %~ markNodeAnnotations
        }

typeAnnotationFromEvalRes :: MonadTransaction n f => EvalPrep -> f (Sugar.Annotation v AddNames.InternalName)
typeAnnotationFromEvalRes x =
    makeTypeAnnotation (x ^. eEvalId) (x ^. eType) <&> Sugar.AnnotationType

makeAnnotation ::
    MonadTransaction n m =>
    Annotations.Mode ->
    (ShowAnnotation, EvalPrep) ->
    m (Sugar.Annotation EvalPrep AddNames.InternalName)
makeAnnotation annMode (showAnn, x) =
    case annMode of
    _ | showAnn ^. showTypeAlways -> typeAnnotationFromEvalRes x
    Annotations.Types | showAnn ^. showInTypeMode -> typeAnnotationFromEvalRes x
    Annotations.Evaluation | showAnn ^. showInEvalMode -> Sugar.AnnotationVal x & pure
    _ -> pure Sugar.AnnotationNone

redirectLams :: [UUID] -> EvalResults -> EvalResults
redirectLams lams results =
    results
    & erExprValues . Lens.mapped %~ Map.mapKeys mapScopeId
    & erAppliesOfLam . Lens.mapped %~ Map.mapKeys mapScopeId
    where
        mapScopeId x = mapping ^. Lens.at x & maybe x mapScopeId
        mapping =
            lams
            >>= (\lamId -> results ^@.. erAppliesOfLam . Lens.ix lamId . Lens.ifolded <. traverse . _1)
            <&> swap
            & Map.fromList

sugarWorkArea ::
    ( HasCallStack
    , Has Debug.Monitors env0
    , Has SugarConfig.Config env0
    , Has Cache.Functions env0
    , Has Annotations.Mode env1
    , Has (Texts.Name Text) env1
    , Has (Texts.Code Text) env1
    , Has (CurAndPrev EvalResults) env1
    , Monad m, Typeable m
    ) =>
    env0 -> Anchors.CodeAnchors m ->
    OnceT (T m)
    ( (Tag -> (IsOperator, TextsInLang)) -> env1 ->
        OnceT (T m) 
        ( Sugar.WorkArea 
            ( Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name
            ) Name (OnceT (T m)) (T m)
            ( Sugar.Payload
                (Sugar.Annotation 
                    ( Sugar.EvaluationScopes Name (OnceT (T m)) )
                    Name
                ) Name (OnceT (T m)) (T m),
                Sugar.GuiPayload
            )
        )
    )
sugarWorkArea env0 cp =
    SugarConvert.loadWorkArea env0 cp
    <&>
    \workArea getTagName env1 ->
    let strippedLams = workArea ^.. SugarLens.workAreaAnnotations . eLambdas . traverse
    in
    markAnnotations (env0 ^. has) workArea
    & SugarLens.workAreaAnnotations (makeAnnotation (env1 ^. has))
    >>= lift . addEvaluationResults cp (env1 ^. has <&> redirectLams strippedLams)
    >>= report . AddNames.addToWorkArea env1 (fmap getTagName . lift . ExprIRef.readTagData)
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~ \(parenInfo, pl) -> pl <&> Sugar.GuiPayload parenInfo
    where
        Debug.EvaluatorM report = env0 ^. has . Debug.naming . Debug.mAction
