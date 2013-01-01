{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ListEdit(make) where

import Control.Lens ((&), (%~), (^.))
import Control.MonadA (MonadA)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

make :: MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.List items mAddFirstItem) =
  ExpressionGui.wrapExpression $ \myId ->
  let
    firstBracketId = Widget.joinId myId ["first-bracket"]
    addFirstElemEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor
       Config.listAddItemKeys (E.Doc ["Edit", "List", "Add First Item"]) .
       fmap WidgetIds.fromGuid)
      mAddFirstItem
    makeFirstBracket txt =
      (ExpressionGui.makeFocusableView firstBracketId =<< makeBracketLabel txt)
      & Lens.mapped . ExpressionGui.egWidget %~
        Widget.weakerEvents addFirstElemEventMap
    cursorDest = maybe firstBracketId itemId $ listToMaybe items
    makeBracketLabel text =
      ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor text myId
  in ExprGuiM.assignCursor myId cursorDest $
    case items of
    [] -> makeFirstBracket "[]"
    _ -> do
      bracketOpen <- makeFirstBracket "["
      let
        closeBracketId = Widget.joinId myId ["close-bracket"]
        mkItemEventMap actions =
          Widget.keysEventMapMovesCursor
          Config.listAddItemKeys (E.Doc ["Edit", "List", "Add Next Item"]) .
          fmap WidgetIds.fromGuid $ actions ^. Sugar.itemAddNext
        mkItem item =
          Lens.sequenceOf Lens.both
          ( ExpressionGui.makeColoredLabel Config.listCommaTextSize
            Config.listCommaColor ", " .
            Widget.augmentId ',' . WidgetIds.fromGuid . Lens.view Sugar.rGuid $
            Sugar.liExpr item
          , ExprGuiM.makeSubexpresion (Sugar.liExpr item)
            & Lens.mapped . ExpressionGui.egWidget %~
              Widget.weakerEvents
              (maybe mempty mkItemEventMap (Sugar.liMActions item))
          )

      bracketClose <- ExpressionGui.makeFocusableView closeBracketId =<< makeBracketLabel "]"
      firstEdit : nextEdits <- mapM mkItem items
      let pairToList (x, y) = [x, y]
      return . ExpressionGui.hbox $ concat
        [[bracketOpen, snd firstEdit], nextEdits >>= pairToList, [bracketClose]]
  where
    itemId = WidgetIds.fromGuid . Lens.view Sugar.rGuid . Sugar.liExpr
