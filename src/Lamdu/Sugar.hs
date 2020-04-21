-- | Top-level wrapper for Sugar.Convert, Sugar.Parens, Sugar.AddNames

module Lamdu.Sugar
    ( sugarWorkArea
    , Sugar.WorkArea, Sugar.Payload, Sugar.ParenInfo, Sugar.EntityId, Name
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Tag (Tag, IsOperator, TextsInLang)
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Lamdu.Sugar.Convert as SugarConvert
import           Lamdu.Sugar.Eval (addEvaluationResults)
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

sugarWorkArea ::
    ( HasCallStack
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has Debug.Monitors env
    , Has (CurAndPrev EvalResults) env
    , Has SugarConfig.Config env
    , Has Cache.Functions env, Has Annotations.Mode env
    , Monad m
    ) =>
    (Tag -> (IsOperator, TextsInLang)) -> env -> Anchors.CodeAnchors m ->
    T m
    (Sugar.WorkArea (Sugar.EvaluationScopes Name (T m)) Name (T m) (T m)
        (Sugar.Payload (Sugar.EvaluationScopes Name (T m)) Name (T m) (T m),
            (Sugar.ParenInfo, [Sugar.EntityId])))
sugarWorkArea getTagName env cp =
    SugarConvert.loadWorkArea env cp
    >>= addEvaluationResults cp (env ^. has)
    >>= report .
        AddNames.addToWorkArea env
        (fmap getTagName . ExprIRef.readTagData)
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~ \(parenInfo, pl) -> pl <&> (,) parenInfo
    where
        Debug.EvaluatorM report = env ^. has . Debug.naming . Debug.mAction
