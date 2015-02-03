{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit(make) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.List as List
import qualified Graphics.UI.Bottle.SizedFont as SizedFont
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

shrinkIfHigherThanLine :: MonadA m => ExpressionGui f -> ExprGuiM m (ExpressionGui f)
shrinkIfHigherThanLine w = do
  sizedFont <-
    ExprGuiM.widgetEnv WE.readTextStyle
    <&> (^. TextEdit.sTextViewStyle . TextView.styleFont)
  config <- ExprGuiM.readConfig <&> Config.hole
  let
    ratio =
      (SizedFont.textHeight sizedFont /
       w ^. ExpressionGui.egWidget . Widget.height)
      ** realToFrac (Config.holeResultInjectedScaleExponent config)
  return $
    if ratio < 1
    then ExpressionGui.scaleFromTop (realToFrac ratio) w
    else w

make ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make parentPrecedence sExpr =
  assignCursor $
  do
    gui <- makeEditor parentPrecedence body pl
    maybeShrink gui <&> ExpressionGui.egWidget %~ maybeDoesntTakeFocus
  where
    maybeDoesntTakeFocus
      | Lens.has Lens._Nothing (pl ^. Sugar.plActions) = Widget.doesntTakeFocus
      | otherwise = id
    Sugar.Expression body pl = sExpr
    exprHiddenEntityIds =
      List.delete (pl ^. Sugar.plEntityId)
      (pl ^. Sugar.plData ^. ExprGuiM.plStoredEntityIds)
    myId = WidgetIds.fromExprPayload pl
    maybeShrink
      | or (pl ^. Sugar.plData ^. ExprGuiM.plInjected) = shrinkIfHigherThanLine
      | otherwise = return
    assignCursor x =
      foldr (`ExprGuiM.assignCursorPrefix` const myId) x $
      exprHiddenEntityIds <&> WidgetIds.fromEntityId

makeEditor ::
  MonadA m => ParentPrecedence ->
  Sugar.Body (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m)
makeEditor parentPrecedence body =
  case body of
  Sugar.BodyHole hole -> HoleEdit.make hole
  Sugar.BodyApply apply -> ApplyEdit.make parentPrecedence apply
  Sugar.BodyLam lam -> LambdaEdit.make parentPrecedence lam
  Sugar.BodyLiteralInteger integer -> LiteralEdit.makeInt integer
  Sugar.BodyList list -> ListEdit.make list
  Sugar.BodyRecord record -> RecordEdit.make record
  Sugar.BodyGetField getField -> GetFieldEdit.make getField
  Sugar.BodyGetVar gv -> GetVarEdit.make gv
