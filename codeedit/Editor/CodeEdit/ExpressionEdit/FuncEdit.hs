module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamEdit) where

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
makeParamEdit :: MonadF m => Sugar.FuncParam m -> TWidget ViewTag m
makeParamEdit param =
  assignCursor (WidgetIds.fromIRef (Sugar.fpLambdaI param)) (WidgetIds.fromIRef (Sugar.fpParamI param)) .
  (liftM . Widget.weakerEvents) paramDeleteEventMap $
  ParamEdit.make (Sugar.fpParamI param)
  where
    paramDeleteEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" .
      liftM WidgetIds.fromIRef $ Sugar.fpRemoveParam param

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Sugar.Func m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr func@(Sugar.Func params bodyPtr) myId = do
  bodyI <- getP bodyPtr
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    bodyEdit <- makeExpressionEdit (ancestryItem : ancestry) bodyPtr
    paramsEdit <- liftM BWidgets.hboxSpaced $ mapM makeParamEdit params
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
  where
    ancestryItem = AncestryItemLambda $ LambdaParent func expressionPtr
