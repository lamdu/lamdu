-- | Top-level wrapper for Sugar.Convert, Sugar.Parens, Sugar.AddNames

module Lamdu.Sugar
    ( sugarWorkArea
    , Sugar.WorkArea, Sugar.Payload, Sugar.ParenInfo, Sugar.EntityId, Name
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Control.Monad.Reader (ReaderT(..))
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
import           Lamdu.Sugar.Convert.Annotation (makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Eval (addEvaluationResults)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

markAnnotations ::
    Functor m =>
    Sugar.WorkArea v n i o (ConvertPayload m a) ->
    Sugar.WorkArea (ShowAnnotation, v) n i o (ConvertPayload m (ShowAnnotation, a))
markAnnotations workArea =
    workArea
    { Sugar._waPanes = workArea ^. Sugar.waPanes <&> SugarLens.paneBinder %~ markNodeAnnotations
    , Sugar._waRepl = workArea ^. Sugar.waRepl & Sugar.replExpr %~ markNodeAnnotations
    }

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
    , Anchors.HasCodeAnchors env0 m
    , Has Annotations.Mode env1
    , Has (Texts.Name Text) env1
    , Has (Texts.Code Text) env1
    , Has (CurAndPrev EvalResults) env1
    , Monad m, Typeable m
    ) =>
    env0 ->
    OnceT (T m)
    ( (Tag -> (IsOperator, TextsInLang)) -> env1 ->
        OnceT (T m) (Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) Name (OnceT (T m)) (T m)
            (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) (T m)))
    )
sugarWorkArea env0 =
    SugarConvert.loadWorkArea env0
    <&>
    \workArea getTagName env1 ->
    let strippedLams = workArea ^.. traverse . pLambdas . traverse
    in
    markAnnotations workArea
    <&> initAnnotationEvalPrep
    & SugarLens.annotations (makeAnnotation (env1 ^. has))
    & (`runReaderT` env0)
    >>= lift . addEvaluationResults (env0 ^. Anchors.codeAnchors) (env1 ^. has <&> redirectLams strippedLams)
    >>= report . AddNames.addToWorkArea env1 (fmap getTagName . lift . ExprIRef.readTagData)
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~
    \(paren, pl) ->
    Sugar.Payload
    { Sugar._plAnnotation = pl ^. pInput . Input.userData . _1
    , Sugar._plActions = pl ^. pActions
    , Sugar._plEntityId = pl ^. pInput . Input.entityId
    , Sugar._plParenInfo = paren
    , Sugar._plHiddenEntityIds = pl ^. pInput . Input.userData . _2
    }
    where
        Debug.EvaluatorM report = env0 ^. has . Debug.naming . Debug.mAction
        initAnnotationEvalPrep pl =
            pl & pInput . Input.userData %~ \(showAnn, x) -> ((showAnn, mkEvalPrep pl), x)

mkEvalPrep :: ConvertPayload m a -> EvalPrep
mkEvalPrep pl =
    EvalPrep
    { _eType = pl ^. pInput . Input.inferredType
    , _eEvalId = pl ^. pInput . Input.entityId
    }
