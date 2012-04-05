module Editor.CodeEdit.ExpressionEdit.FuncEdit(make, makeParamEdit) where

import Control.Monad (liftM)
import Data.Store.IRef(IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, atTextSizeColor)
import Editor.CodeEdit.Types(AncestryItem(..), LambdaParent(..))
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

makeParamNameEdit
  :: Monad m
  => IRef Data.Parameter
  -> TWidget t m
makeParamNameEdit paramI =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.parameterColor .
   BWidgets.makeNameEdit "<unnamed param>" paramI) $
  WidgetIds.fromIRef paramI

-- makeParamsEdit exported for use in definition sugaring.
makeParamEdit
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> Sugar.FuncParam m
  -> TWidget ViewTag m
makeParamEdit makeExpressionEdit ancestry param = do
  lambdaI <- getP $ Sugar.fpLambdaPtr param
  let paramI = Sugar.fpParamI param
  assignCursor (WidgetIds.fromIRef lambdaI) (WidgetIds.fromIRef paramI) .
    (liftM . Widget.weakerEvents) paramDeleteEventMap $ do
    paramNameEdit <- makeParamNameEdit (Sugar.fpParamI param)
    let newAncestryItem = ETypes.AncestryItemParamType $ ETypes.ParamTypeParent paramI
    paramTypeEdit <- makeExpressionEdit (newAncestryItem : ancestry) (Sugar.fpTypePtr param)
    let shrunkParamTypeEdit = Widget.scale Config.typeScaleFactor paramTypeEdit
    return $ BWidgets.vbox [ paramNameEdit, shrunkParamTypeEdit ]
  where
    paramDeleteEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete parameter" $ do
        Property.set (Sugar.fpLambdaPtr param) (Sugar.fpBodyI param)
        return . WidgetIds.fromIRef $ Sugar.fpBodyI param

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
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
    paramsEdit <-
      liftM BWidgets.hboxSpaced $
      mapM (makeParamEdit makeExpressionEdit ancestry) params
    return $ BWidgets.hbox [lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit]
