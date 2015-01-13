{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

shrinkIfHigherThanLine :: MonadA m => ExpressionGui f -> ExprGuiM m (ExpressionGui f)
shrinkIfHigherThanLine w = do
  fontSize <-
    (^. TextEdit.sTextViewStyle . TextView.styleFontSize) <$>
    ExprGuiM.widgetEnv WE.readTextStyle
  config <- Config.hole <$> ExprGuiM.widgetEnv WE.readConfig
  let
    textHeight = fromIntegral fontSize * DrawUtils.textHeight
    ratio =
      (textHeight / w ^. ExpressionGui.egWidget . Widget.wHeight)
      ** realToFrac (Config.holeResultInjectedScaleExponent config)
  return $
    if ratio < 1
    then ExpressionGui.scaleFromTop (realToFrac ratio) w
    else w

make ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make parentPrecedence sExpr = assignCursor $ do
  gui <- makeEditor parentPrecedence body pl myId
  maybeShrink gui <&> ExpressionGui.egWidget %~ maybeDoesntTakeFocus
  where
    maybeDoesntTakeFocus
      | Lens.has Lens._Nothing (pl ^. Sugar.plActions) = Widget.doesntTakeFocus
      | otherwise = id
    Sugar.Expression body pl = sExpr
    exprHiddenEntityIds =
      List.delete (pl ^. Sugar.plEntityId)
      (pl ^. Sugar.plData ^. ExprGuiM.plStoredEntityIds)
    myId = WidgetIds.fromEntityId $ pl ^. Sugar.plEntityId
    maybeShrink
      | or (pl ^. Sugar.plData ^. ExprGuiM.plInjected) = shrinkIfHigherThanLine
      | otherwise = return
    assignCursor x =
      foldr (`ExprGuiM.assignCursorPrefix` myId) x $
      WidgetIds.fromEntityId <$> exprHiddenEntityIds

makeEditor ::
  MonadA m => ParentPrecedence ->
  Sugar.Body (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
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
