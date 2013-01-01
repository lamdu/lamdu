{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ListEdit(make) where

import Control.MonadA (MonadA)
import Data.Maybe (listToMaybe)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

make :: MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.List items) =
  ExpressionGui.wrapExpression $ \myId ->
  let
    openBracketId = Widget.joinId myId ["open-bracket"]
    closeBracketId = Widget.joinId myId ["close-bracket"]
    cursorDest = maybe openBracketId itemId $ listToMaybe items
    makeBracketLabel text =
      ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor text myId
  in ExprGuiM.assignCursor myId cursorDest $ do
    bracketOpen <- ExpressionGui.makeFocusableView openBracketId =<< makeBracketLabel "["
    bracketClose <- ExpressionGui.makeFocusableView closeBracketId =<< makeBracketLabel "]"
    exprEdits <- mapM (ExprGuiM.makeSubexpresion . Sugar.liExpr) items
    comma <- ExpressionGui.makeColoredLabel Config.listCommaTextSize Config.listCommaColor ", " myId
    return . ExpressionGui.hbox $ concat
      [[bracketOpen], List.intersperse comma exprEdits, [bracketClose]]
  where
    itemId = WidgetIds.fromGuid . Lens.view Sugar.rGuid . Sugar.liExpr
