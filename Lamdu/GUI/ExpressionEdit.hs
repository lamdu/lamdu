{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.GUI.ExpressionEdit.CollapsedEdit as CollapsedEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetParamsEdit as GetParamsEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.PiEdit as PiEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

shrinkIfHigherThanLine :: MonadA m => ExpressionGui f -> ExprGuiM m (ExpressionGui f)
shrinkIfHigherThanLine w = do
  fontSize <-
    (^. TextEdit.sTextViewStyle . TextView.styleFontSize) <$>
    ExprGuiM.widgetEnv WE.readTextStyle
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    textHeight = fromIntegral fontSize * DrawUtils.textHeight
    ratio =
      (textHeight / w ^. ExpressionGui.egWidget . Widget.wSize . Lens._2)
      ** realToFrac (Config.holeResultInjectedScaleExponent config)
  return $
    if ratio < 1
    then ExpressionGui.scaleFromTop (realToFrac ratio) w
    else w

make ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make parentPrecedence sExpr = assignCursor $ do
  (gui, resultPickers) <- ExprGuiM.listenResultPickers $ makeEditor parentPrecedence sExpr
  exprEventMap <- ExprEventMap.make resultPickers sExpr
  maybeShrink gui
    <&>
    ExpressionGui.egWidget %~
    ( maybe Widget.doesntTakeFocus (const id) (pl ^. Sugar.plActions)
    . Widget.weakerEvents exprEventMap
    )
  where
    pl = sExpr ^. Sugar.rPayload
    ExprGuiM.Payload guids isInjecteds _holeGuids = pl ^. Sugar.plData
    exprHiddenGuids = List.delete (pl ^. Sugar.plGuid) guids
    exprId = WidgetIds.fromGuid $ pl ^. Sugar.plGuid
    maybeShrink
      | or isInjecteds = shrinkIfHigherThanLine
      | otherwise = return
    assignCursor f =
      foldr (`ExprGuiM.assignCursorPrefix` exprId) f $
      WidgetIds.fromGuid <$> exprHiddenGuids

makeEditor ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m ->
  ExprGuiM m (ExpressionGui m)
makeEditor parentPrecedence sExpr =
  ( case sExpr ^. Sugar.rBody of
    Sugar.BodyInferred i -> InferredEdit.make parentPrecedence i
    Sugar.BodyHole hole -> HoleEdit.make hole
    Sugar.BodyCollapsed poly -> const (CollapsedEdit.make parentPrecedence poly)
    Sugar.BodyApply apply -> ApplyEdit.make parentPrecedence apply
    Sugar.BodyLam lam@(Sugar.Lam Sugar.KType _ _ _) -> PiEdit.make parentPrecedence lam
    Sugar.BodyLam lam@(Sugar.Lam Sugar.KVal _ _ _) -> LambdaEdit.make parentPrecedence lam
    Sugar.BodyLiteralInteger integer -> LiteralEdit.makeInt integer
    Sugar.BodyAtom atom -> const (AtomEdit.make atom)
    Sugar.BodyList list -> ListEdit.make list
    Sugar.BodyRecord record -> RecordEdit.make record
    Sugar.BodyGetField getField -> GetFieldEdit.make getField
    Sugar.BodyTag tag -> TagEdit.make tag
    Sugar.BodyGetVar gv -> GetVarEdit.make gv
    Sugar.BodyGetParams gp -> GetParamsEdit.make gp
  ) pl myId
  where
    myId = WidgetIds.fromGuid $ pl ^. Sugar.plGuid
    pl = sExpr ^. Sugar.rPayload
