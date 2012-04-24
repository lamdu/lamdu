module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamsEdit) where

import Control.Monad (liftM)
import Data.Store.IRef (IRef)
import Data.Vector.Vector2 (Vector2(Vector2))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, WidgetT, getP, assignCursor, atTextSizeColor)
import Editor.CodeEdit.Ancestry (AncestryItem(..), LambdaParent(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.DataOps (ExpressionPtr)
import Editor.MonadF (MonadF)
import qualified Data.List as List
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

makeParamNameEdit
  :: Monad m
  => IRef Data.Parameter
  -> TWidget t m
makeParamNameEdit paramI =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.parameterColor .
   BWidgets.makeNameEdit "<unnamed param>" paramI) $
  WidgetIds.fromIRef paramI

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

makeParamEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> Sugar.FuncParam m
  -> CTransaction ViewTag m (WidgetT ViewTag m, WidgetT ViewTag m)
makeParamEdit makeExpressionEdit ancestry param = do
  lambdaI <- getP $ Sugar.fpLambdaPtr param
  let paramI = Sugar.fpParamI param
  assignCursor (WidgetIds.fromIRef lambdaI) (WidgetIds.fromIRef paramI) .
    (liftM . both . Widget.weakerEvents) paramDeleteEventMap $ do
    paramNameEdit <-
      liftM (Widget.align down) $
      makeParamNameEdit (Sugar.fpParamI param)
    let newAncestryItem = Ancestry.AncestryItemParamType $ Ancestry.ParamTypeParent paramI
    paramTypeEdit <-
      liftM
      (Widget.scale Config.typeScaleFactor .
       Widget.align up) $
      makeExpressionEdit (newAncestryItem : ancestry) (Sugar.fpTypePtr param)
    return (paramNameEdit, paramTypeEdit)
  where
    up = Vector2 0.5 0
    down = Vector2 0.5 1
    paramDeleteEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" $ do
        Property.set (Sugar.fpLambdaPtr param) (Sugar.fpBodyI param)
        return . WidgetIds.fromIRef $ Sugar.fpBodyI param

paramTypesGrid :: [(WidgetT t m, WidgetT t m)] -> WidgetT t m
paramTypesGrid paramTypes =
  Grid.toWidget $ Grid.make [params, types]
  where
    (params, types) = unzip $ List.intersperse space paramTypes
    space = (BWidgets.spaceWidget, BWidgets.spaceWidget)

-- exported for use in definition sugaring.
makeParamsEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeParamsEdit makeExpressionEdit ancestry =
  liftM paramTypesGrid .
  mapM (makeParamEdit makeExpressionEdit ancestry)

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> ExpressionPtr m
  -> Sugar.Func m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr func@(Sugar.Func params bodyPtr) myId = do
  exprI <- getP expressionPtr
  bodyI <- getP bodyPtr
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    bodyEdit <- makeExpressionEdit (AncestryItemLambda (LambdaParent func exprI) : ancestry) bodyPtr
    paramsEdit <- makeParamsEdit makeExpressionEdit ancestry params
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
