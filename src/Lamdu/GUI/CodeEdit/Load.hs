{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Load the sugared code

module Lamdu.GUI.CodeEdit.Load
    ( loadWorkArea
    ) where

import qualified Control.Lens as Lens
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
import qualified Lamdu.Sugar.Config as SugarConf
import qualified Lamdu.Sugar.Convert as SugarConvert
import           Lamdu.Sugar.Convert.Input (AnnotationMode)
import qualified Lamdu.Sugar.Names.Add as AddNames
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

toExprGuiMPayload ::
    ( AddParens.MinOpPrec, AddParens.NeedsParens
    , Sugar.Payload name i o [Sugar.EntityId]
    ) -> Sugar.Payload name i o ExprGui.Payload
toExprGuiMPayload (minOpPrec, needParens, pl) =
    pl <&>
    \entityIds ->
    ExprGui.Payload entityIds
    (needParens == AddParens.NeedsParens)
    minOpPrec

getNameProp :: Monad m => Anchors.CodeAnchors m -> T.Tag -> MkProperty' (T m) Text
getNameProp = DataOps.assocPublishedTagName . Anchors.tags

loadWorkArea ::
    (HasCallStack, Monad m) =>
    SugarConf.Config -> Cache.Functions -> Debug.Monitors ->
    AnnotationMode -> CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m ->
    T m
    (Sugar.WorkArea (Name (T m)) (T m) (T m)
        (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload))
loadWorkArea config cache monitors annMode  theEvalResults cp =
    SugarConvert.loadWorkArea config cache monitors annMode theEvalResults cp
    >>= report . AddNames.addToWorkArea (getNameProp cp)
    <&> AddParens.addToWorkArea
    <&> Lens.mapped %~ toExprGuiMPayload
    where
        Debug.EvaluatorM report = monitors ^. Debug.naming . Debug.mAction
