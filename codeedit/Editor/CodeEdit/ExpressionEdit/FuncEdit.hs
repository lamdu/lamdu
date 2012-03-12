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
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

-- makeParamsEdit exported for use in definition sugaring.
makeParamsEdit :: MonadF m => Sugar.Func m -> TWidget ViewTag m
makeParamsEdit (Sugar.Func params _) = do
  paramEdits <- mapM ParamEdit.make params
  return $ BWidgets.hbox paramEdits

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
    let
      -- TODO: support deleting individual params instead..
      paramsEditEventMap =
        Widget.actionEventMapMovesCursor Config.delKeys "Remove lambda wrapper" $
        setExpr bodyI
    return $ BWidgets.hbox [
      lambdaLabel,
      Widget.weakerEvents paramsEditEventMap paramsEdit,
      rightArrowLabel,
      bodyEdit
      ]
  where
    ancestryItem = AncestryItemLambda $ LambdaParent func expressionPtr
    setExpr = liftM WidgetIds.fromIRef . DataOps.replace expressionPtr
