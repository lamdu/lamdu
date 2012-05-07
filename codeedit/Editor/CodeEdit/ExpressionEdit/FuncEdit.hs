module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamEdit) where

import Control.Monad (liftM)
import Data.List.Utils (pairList)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Vector.Vector2 (Vector2(Vector2))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, WidgetT, getP, assignCursor, atTextSizeColor)
import Editor.CodeEdit.Ancestry (AncestryItem(..), LambdaParent(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.DataOps (ExpressionPtr)
import Editor.MonadF (MonadF)
import qualified Data.List as List
import qualified Data.Store.IRef as IRef
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

makeParamNameEdit
  :: Monad m
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
  -> Ancestry.ExpressionAncestry m
  -> Sugar.FuncParam m
  -> CTransaction ViewTag m (WidgetT ViewTag m, WidgetT ViewTag m)
makeParamEdit makeExpressionEdit ancestry param = do
  assignCursor (WidgetIds.fromGuid ident) (WidgetIds.paramId ident) .
    (liftM . both . Widget.weakerEvents) paramDeleteEventMap $ do
    paramNameEdit <-
      liftM (Widget.align down) $
      makeParamNameEdit ident
    let
      newAncestryItem =
        Ancestry.AncestryItemParamType $
        -- TODO: don't use IRef.unsafeFromGuid
        Ancestry.ParamTypeParent (IRef.unsafeFromGuid ident)
    paramTypeEdit <-
      liftM
      (Widget.scale Config.typeScaleFactor .
       Widget.align up) $
      makeExpressionEdit (newAncestryItem : ancestry) (Sugar.fpType param)
    return (paramNameEdit, paramTypeEdit)
  where
    ident = Sugar.guid $ Sugar.fpActions param
    up = Vector2 0.5 0
    down = Vector2 0.5 1
    paramDeleteEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" . liftM WidgetIds.fromGuid)
      (Sugar.mDelete (Sugar.fpActions param))

makeParamsEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeParamsEdit makeExpressionEdit ancestry =
  liftM (BWidgets.gridHSpaced . List.transpose . map pairList) .
  mapM (makeParamEdit makeExpressionEdit ancestry)

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> ExpressionPtr m
  -> Sugar.Func m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr func@(Sugar.Func params body) myId = do
  exprI <- getP expressionPtr
  bodyI <- getP $ Sugar.rExpressionPtr body
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    bodyEdit <- makeExpressionEdit (AncestryItemLambda (LambdaParent func exprI) : ancestry) body
    paramsEdit <- makeParamsEdit makeExpressionEdit ancestry params
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
