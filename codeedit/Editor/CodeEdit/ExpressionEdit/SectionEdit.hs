{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.Section m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Section mLArg op mRArg) myId = do
  cursorPosI <- getP . Sugar.rExpressionPtr . head $ catMaybes [mRArg, Just op, mLArg]
  assignCursor myId (WidgetIds.fromIRef cursorPosI) $ do
    let fromMArg = maybe (return []) $ liftM (:[]) . makeExpressionEdit
    lArgEdit <- fromMArg mLArg
    opEdit <- liftM (:[]) $ makeExpressionEdit op
    rArgEdit <- fromMArg mRArg
    return . BWidgets.hboxSpaced $ lArgEdit ++ opEdit ++ rArgEdit
