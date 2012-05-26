module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamEdit) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Utils (pairList)
import Data.Monoid (mempty, mconcat)
import Data.Store.Guid (Guid)
import Data.Vector.Vector2 (Vector2(Vector2))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, WidgetT, assignCursor, atTextSizeColor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

makeParamNameEdit
  :: MonadF m
  => Guid
  -> TWidget t m
makeParamNameEdit ident =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.parameterColor .
   BWidgets.makeNameEdit "<unnamed param>" ident) $
  WidgetIds.paramId ident

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- exported for use in definition sugaring.
makeParamEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.FuncParam m
  -> CTransaction ViewTag m (WidgetT ViewTag m, WidgetT ViewTag m)
makeParamEdit makeExpressionEdit param =
  assignCursor (WidgetIds.fromGuid ident) (WidgetIds.paramId ident) .
    (liftM . both . Widget.weakerEvents) paramEventMap $ do
    paramNameEdit <- makeParamNameEdit ident
    paramTypeEdit <- makeExpressionEdit $ Sugar.fpType param
    return
      (Widget.align down paramNameEdit,
       Widget.align up paramTypeEdit)
  where
    ident = Sugar.guid $ Sugar.fpActions param
    up = Vector2 0.5 0
    down = Vector2 0.5 1
    paramEventMap = mconcat
      [ paramDeleteEventMap
      , paramAddNextEventMap
      ]
    paramAddNextEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.addNextParamKeys "Add next parameter" .
       liftM (WidgetIds.delegating . WidgetIds.paramId)) .
      Sugar.lambdaWrap . Sugar.rActions $ Sugar.fpBody param
    paramDeleteEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" .
       liftM WidgetIds.fromGuid) .
      Sugar.mDelete $ Sugar.fpActions param

makeParamsEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeParamsEdit makeExpressionEdit =
  liftM (BWidgets.gridHSpaced . List.transpose . map (pairList . scaleDownType)) .
  mapM (makeParamEdit makeExpressionEdit)
  where
    scaleDownType = second $ Widget.scale Config.typeScaleFactor

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.Func m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Func params body) myId =
  assignCursor myId ((WidgetIds.fromGuid . Sugar.guid . Sugar.rActions) body) $ do
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    bodyEdit <- makeExpressionEdit body
    paramsEdit <- makeParamsEdit makeExpressionEdit params
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
