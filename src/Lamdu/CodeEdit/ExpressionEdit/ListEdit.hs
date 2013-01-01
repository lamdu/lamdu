{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ListEdit(make) where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM)
import Control.MonadA (MonadA)
import Data.Maybe (listToMaybe)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
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
    firstBracketId = Widget.joinId myId ["first-bracket"]
    cursorDest = maybe firstBracketId itemId $ listToMaybe items
    makeBracketLabel text =
      ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor text myId
  in ExprGuiM.assignCursor myId cursorDest $
    case items of
    [] -> ExpressionGui.makeFocusableView firstBracketId =<< makeBracketLabel "[]"
    _ -> do
      bracketOpen <- ExpressionGui.makeFocusableView firstBracketId =<< makeBracketLabel "["
      let
        closeBracketId = Widget.joinId myId ["close-bracket"]
        addComma x i =
          (: [x]) <$>
          ExpressionGui.makeColoredLabel Config.listCommaTextSize
          Config.listCommaColor ", " (Widget.augmentId i myId)
      bracketClose <- ExpressionGui.makeFocusableView closeBracketId =<< makeBracketLabel "]"
      firstEdit : nextEdits <- mapM (ExprGuiM.makeSubexpresion . Sugar.liExpr) items
      nextEditsWithCommas <- concat <$> zipWithM addComma nextEdits [(0::Int)..]
      return . ExpressionGui.hbox $ concat
        [[bracketOpen, firstEdit], nextEditsWithCommas, [bracketClose]]
  where
    itemId = WidgetIds.fromGuid . Lens.view Sugar.rGuid . Sugar.liExpr
