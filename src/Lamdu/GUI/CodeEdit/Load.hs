{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Load the sugared code

module Lamdu.GUI.CodeEdit.Load
    ( loadWorkArea
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.I18N.Code as Texts
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

toGuiMPayload ::
    ( AddParens.MinOpPrec, AddParens.NeedsParens
    , Sugar.Payload name i o [Sugar.EntityId]
    ) -> Sugar.Payload name i o ExprGui.Payload
toGuiMPayload (minOpPrec, needParens, pl) =
    pl <&>
    \entityIds ->
    ExprGui.Payload entityIds
    (needParens == AddParens.NeedsParens)
    minOpPrec

loadWorkArea ::
    ( HasCallStack
    , Has LangId env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has SugarConfig.Config env
    , Has Cache.Functions env, Has Annotations.Mode env
    , Monad m
    ) =>
    env -> Anchors.CodeAnchors m ->
    T m
    (Sugar.WorkArea (Name (T m)) (T m) (T m)
        (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload))
loadWorkArea env cp =
    SugarConvert.loadWorkArea env cp
    >>= report . AddNames.addToWorkArea env (DataOps.assocTagName env)
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~ toGuiMPayload
    where
        Debug.EvaluatorM report = env ^. has . Debug.naming . Debug.mAction
