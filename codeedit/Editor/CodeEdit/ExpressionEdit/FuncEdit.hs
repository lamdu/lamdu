module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamsEdit) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.Types(AncestryItem(..), LambdaParent(..))
import Editor.CTransaction (TWidget, getP, assignCursor, atTextSizeColor)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

-- makeParamsEdit exported for use in definition sugaring.
makeParamsEdit :: MonadF m => Sugar.Func m -> TWidget ViewTag m
makeParamsEdit (Sugar.Func params _) = do
  paramEdits <- mapM makeParamEdit params
  return $ BWidgets.hbox paramEdits
  where
    makeParamEdit param =
      assignCursor (WidgetIds.fromIRef (Sugar.fpLambdaI param)) (WidgetIds.fromIRef (Sugar.fpParamI param)) .
      (liftM . Widget.weakerEvents) (paramDeleteEventMap param) $
      ParamEdit.make (Sugar.fpParamI param)
    paramDeleteEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" .
      liftM WidgetIds.fromIRef . Sugar.fpRemoveParam

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Sugar.Func m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr func@(Sugar.Func _ bodyPtr) myId = do
  bodyI <- getP bodyPtr
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    bodyEdit <- makeExpressionEdit (ancestryItem : ancestry) bodyPtr
    paramsEdit <- makeParamsEdit func
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
  where
    ancestryItem = AncestryItemLambda $ LambdaParent func expressionPtr
