{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Load the sugared code

module Lamdu.GUI.CodeEdit.Load
    ( loadWorkArea
    ) where

import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Property (MkProperty')
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Convert as SugarConvert
import           Lamdu.Sugar.Convert.Input (AnnotationMode)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

toExprGuiMPayload ::
    ( AddParens.MinOpPrec, AddParens.NeedsParens
    , Sugar.Payload name i o ([Sugar.EntityId], NearestHoles)
    ) -> Sugar.Payload name i o ExprGui.Payload
toExprGuiMPayload (minOpPrec, needParens, pl) =
    pl <&>
    \(entityIds, nearestHoles) ->
    ExprGui.Payload entityIds nearestHoles
    (needParens == AddParens.NeedsParens)
    minOpPrec

getNameProp :: Monad m => Anchors.CodeAnchors m -> T.Tag -> MkProperty' (T m) Text
getNameProp = DataOps.assocPublishedTagName . Anchors.tags

loadWorkArea ::
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors ->
    AnnotationMode -> CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m ->
    T m
    (Sugar.WorkArea (Name (T m)) (T m) (T m)
        (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload))
loadWorkArea cache monitors annMode  theEvalResults cp =
    SugarConvert.loadWorkArea cache monitors annMode theEvalResults cp
    >>= report . AddNames.addToWorkArea (getNameProp cp)
    <&>
    \Sugar.WorkArea { _waPanes, _waRepl, _waGlobals } ->
    Sugar.WorkArea
    { _waPanes =
        _waPanes
        <&> Sugar.paneDefinition . Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent %~
            NearestHoles.add SugarLens.assignmentPayloads
    , _waRepl =
        _waRepl
        & Sugar.replExpr %~ NearestHoles.add SugarLens.binderPayloads
    , _waGlobals = _waGlobals
    }
    & AddParens.addToWorkArea
    <&> toExprGuiMPayload
    where
        Debug.EvaluatorM report = monitors ^. Debug.naming . Debug.mAction
