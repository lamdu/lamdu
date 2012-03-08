module Editor.CodeEdit.ExpressionEdit.WhereEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit ancestry (Sugar.Where items bodyI) myId = do
    whereLabel <- BWidgets.makeLabel "where" myId
    bodyEdit <- makeExpressionEdit ancestry bodyI
    whereEdits <- mapM makeWhereItemEdits items
    return . BWidgets.vbox $ [
        bodyEdit,
        whereLabel
        ] ++ whereEdits
    where
        makeWhereItemEdits (paramI, exprI) = do
            paramEdit <- ParamEdit.make paramI
            equalsLabel <- BWidgets.makeLabel "=" $ WidgetIds.fromIRef paramI
            exprEdit <- makeExpressionEdit [] exprI
            return $ BWidgets.hbox [paramEdit, equalsLabel, exprEdit]