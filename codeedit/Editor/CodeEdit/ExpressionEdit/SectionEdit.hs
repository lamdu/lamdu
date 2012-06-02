{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, assignCursor)
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
make makeExpressionEdit (Sugar.Section mLArg op mRArg innerApplyGuid) myId =
  assignCursor myId destId .
    maybe id ((`assignCursor` destId) . WidgetIds.fromGuid) innerApplyGuid $ do
      lArgEdits <- fromMArg mLArg
      opEdits <- makeExpressionsEdit op
      rArgEdits <- fromMArg mRArg
      return . BWidgets.hboxSpaced $ lArgEdits ++ opEdits ++ rArgEdits
  where
    destId = WidgetIds.fromGuid . Sugar.guid . Sugar.rActions $ fromMaybe op mRArg
    makeExpressionsEdit = liftM (:[]) . makeExpressionEdit
    fromMArg = maybe (return []) makeExpressionsEdit
