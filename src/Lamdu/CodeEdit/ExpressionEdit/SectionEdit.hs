{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.WidgetIds as WidgetIds

-- TODO: Parenification is wrong but this module will die
make ::
  MonadA m => ExpressionGui.ParentPrecedence ->
  Sugar.Section (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Section mLArg op mRArg) =
  wrap $ \myId -> ExprGuiM.assignCursor myId destId $ do
    lArgEdits <- fromMArg mLArg
    opEdits <- makeExpressionsEdit 12 op
    rArgEdits <- fromMArg mRArg
    return . ExpressionGui.hboxSpaced $ lArgEdits ++ opEdits ++ rArgEdits
  where
    wrap = case (mLArg, mRArg) of
      (Nothing, Nothing) ->
        ExpressionGui.parenify parentPrecedence (ExpressionGui.MyPrecedence 5) (Parens.addTextParens . Widget.toAnimId)
      _ ->
        ExpressionGui.wrapParenify parentPrecedence (ExpressionGui.MyPrecedence 5) Parens.addHighlightedTextParens
    destId = WidgetIds.fromGuid $ fromMaybe op mRArg ^. Sugar.rGuid
    makeExpressionsEdit prec = fmap (:[]) . ExprGuiM.makeSubexpresion prec
    fromMArg = maybe (return []) (makeExpressionsEdit 6)
