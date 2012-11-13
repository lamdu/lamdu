{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => Sugar.HasParens
  -> Sugar.Section (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Sugar.Section mLArg op mRArg) =
  wrap $ \myId -> ExprGuiM.assignCursor myId destId $ do
    lArgEdits <- fromMArg mLArg
    opEdits <- makeExpressionsEdit op
    rArgEdits <- fromMArg mRArg
    return . ExpressionGui.hboxSpaced $ lArgEdits ++ opEdits ++ rArgEdits
  where
    wrap = case (mLArg, mRArg) of
      (Nothing, Nothing) ->
        ExpressionGui.parenify hasParens (Parens.addTextParens . Widget.toAnimId)
      _ ->
        ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens
    destId = WidgetIds.fromGuid $ fromMaybe op mRArg ^. Sugar.rGuid
    makeExpressionsEdit = liftM (:[]) . ExprGuiM.makeSubexpresion
    fromMArg = maybe (return []) makeExpressionsEdit
