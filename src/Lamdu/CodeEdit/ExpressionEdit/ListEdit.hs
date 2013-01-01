{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ListEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens ((&), (%~), (^.))
import Control.MonadA (MonadA)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
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

make ::
  MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make list = ExpressionGui.wrapExpression $ makeUnwrapped list

makeUnwrapped ::
  MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.List items mAddFirstItem) myId =
  ExprGuiM.assignCursor myId cursorDest $
  mapM makeItem items >>= \itemEdits ->
  case itemEdits of
  [] -> makeFirstBracket "[]"
  (_, firstEdit) : nextEdits -> do
    bracketOpen <- makeFirstBracket "["
    bracketClose <- ExpressionGui.makeFocusableView closeBracketId =<< makeBracketLabel "]"
    return . ExpressionGui.hbox $ concat
      [[bracketOpen, firstEdit], nextEdits >>= pairToList, [bracketClose]]
  where
    pairToList (x, y) = [x, y]
    closeBracketId = Widget.joinId myId ["close-bracket"]
    itemId = WidgetIds.fromGuid . Lens.view Sugar.rGuid . Sugar.liExpr
    addFirstElemEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor
       Config.listAddItemKeys (E.Doc ["Edit", "List", "Add First Item"]) .
       fmap WidgetIds.fromGuid)
      mAddFirstItem
    firstBracketId = Widget.joinId myId ["first-bracket"]
    makeFirstBracket txt =
      (ExpressionGui.makeFocusableView firstBracketId =<< makeBracketLabel txt)
      & Lens.mapped . ExpressionGui.egWidget %~
        Widget.weakerEvents addFirstElemEventMap
    cursorDest = maybe firstBracketId itemId $ listToMaybe items
    makeBracketLabel text =
      ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor text myId

makeItem ::
  MonadA m =>
  Sugar.ListItem m (Sugar.ExpressionP m (Sugar.Payload m)) ->
  ExprGuiM m (ExpressionGui m, ExpressionGui m)
makeItem item =
  (compose . map assignCursor . Sugar.liHiddenGuids) item $
  Lens.sequenceOf Lens.both
  ( ExpressionGui.makeColoredLabel Config.listCommaTextSize
    Config.listCommaColor ", " $
    Widget.augmentId ',' itemWidgetId
  , ExprGuiM.makeSubexpresion itemExpr
    & Lens.mapped . ExpressionGui.egWidget %~
      Widget.weakerEvents
      (maybe mempty mkItemEventMap (Sugar.liMActions item))
  )
  where
    assignCursor guid =
      ExprGuiM.assignCursorPrefix (WidgetIds.fromGuid guid) itemWidgetId
    itemExpr = Sugar.liExpr item
    itemWidgetId = WidgetIds.fromGuid $ itemExpr ^. Sugar.rGuid
    mkItemEventMap Sugar.ListItemActions
      { Sugar._itemAddNext = addItem
      , Sugar._itemDelete = delItem
      } =
      mconcat
      [ Widget.keysEventMapMovesCursor
        Config.listAddItemKeys (E.Doc ["Edit", "List", "Add Next Item"]) $
        WidgetIds.fromGuid <$> addItem
      , Widget.keysEventMapMovesCursor
        (Config.delBackwordKeys ++ Config.delForwardKeys)
        (E.Doc ["Edit", "List", "Delete Item"]) $
        WidgetIds.fromGuid <$> delItem
      ]

compose :: [a -> a] -> a -> a
compose = foldr (.) id
