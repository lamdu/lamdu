-- | Top-level wrapper for Sugar.Convert, Sugar.Parens, Sugar.AddNames

module Lamdu.Sugar
    ( sugarWorkArea
    , Sugar.WorkArea, Sugar.Payload, Sugar.ParenInfo, Sugar.EntityId, Name
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
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
    , Has Debug.Monitors env0
    , Has SugarConfig.Config env0
    , Has Cache.Functions env0, Has Annotations.Mode env0
    , Has (Texts.Name Text) env1
    , Has (Texts.Code Text) env1
    , Has (CurAndPrev EvalResults) env1
    , Monad m
    ) =>
    env0 -> Anchors.CodeAnchors m ->
    OnceT (T m)
    ( (Tag -> (IsOperator, TextsInLang)) -> env1 ->
        OnceT (T m) (Sugar.WorkArea (Sugar.EvaluationScopes Name (OnceT (T m))) Name (OnceT (T m)) (T m)
            (Sugar.Payload (Sugar.EvaluationScopes Name (OnceT (T m))) Name (OnceT (T m)) (T m),
                (Sugar.ParenInfo, [Sugar.EntityId])))
    )
sugarWorkArea env0 cp =
    SugarConvert.loadWorkArea env0 cp
    <&>
    \workArea getTagName env1 ->
    addEvaluationResults cp (env1 ^. has) workArea & lift
    >>= report .
        AddNames.addToWorkArea env1
        (fmap getTagName . (lift . ExprIRef.readTagData))
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~ \(parenInfo, pl) -> pl <&> (,) parenInfo
    where
        Debug.EvaluatorM report = env0 ^. has . Debug.naming . Debug.mAction
