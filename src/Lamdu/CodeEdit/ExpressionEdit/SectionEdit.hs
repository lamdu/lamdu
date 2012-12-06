{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Control.MonadA (MonadA)
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadA m
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
    makeExpressionsEdit = fmap (:[]) . ExprGuiM.makeSubexpresion
    fromMArg = maybe (return []) makeExpressionsEdit
