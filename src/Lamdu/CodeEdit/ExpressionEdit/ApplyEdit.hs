{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Data as Data
import qualified Lamdu.WidgetIds as WidgetIds

make
  :: MonadA m
  => Sugar.HasParens
  -> Data.Apply (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Data.Apply func arg) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  (ExprGuiM.assignCursor myId . WidgetIds.fromGuid) (arg ^. Sugar.rGuid) $ do
    funcEdit <- ExprGuiM.makeSubexpresion func
    argEdit <- ExprGuiM.makeSubexpresion arg
    return $ ExpressionGui.hboxSpaced [funcEdit, argEdit]
