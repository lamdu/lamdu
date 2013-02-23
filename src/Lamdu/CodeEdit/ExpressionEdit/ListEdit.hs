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
make = ExpressionGui.wrapExpression . makeUnwrapped

makeBracketLabel :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui f)
makeBracketLabel =
  ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor

makeUnwrapped ::
  MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.List items mActions) myId =
  ExprGuiM.assignCursor myId cursorDest $
  mapM makeItem items >>= \itemEdits -> do
    bracketOpenLabel <- makeBracketLabel "[" myId
    bracketCloseLabel <- makeBracketLabel "]" myId
    case itemEdits of
      [] ->
        onFirstBracket $ ExpressionGui.hbox [bracketOpenLabel, bracketCloseLabel]
      (_, firstEdit) : nextEdits -> do
        bracketOpen <- onFirstBracket bracketOpenLabel
        bracketClose <-
          ExpressionGui.makeFocusableView closeBracketId bracketCloseLabel
          & Lens.mapped . ExpressionGui.egWidget %~ Widget.weakerEvents nilDeleteEventMap
        return . ExpressionGui.hbox $ concat
          [[bracketOpen, firstEdit], nextEdits >>= pairToList, [bracketClose]]
  where
    pairToList (x, y) = [x, y]
    closeBracketId = Widget.joinId myId ["close-bracket"]
    itemId = WidgetIds.fromGuid . Lens.view Sugar.rGuid . Sugar.liExpr
    actionEventMap keys doc actSelect =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "List", doc]) .
       fmap WidgetIds.fromGuid . actSelect) mActions
    addFirstElemEventMap =
      actionEventMap Config.listAddItemKeys "Add First Item" Sugar.addFirstItem
    delKeys = Config.delForwardKeys ++ Config.delBackwordKeys
    nilDeleteEventMap =
      actionEventMap delKeys "Replace nil with hole" Sugar.replaceNil
    firstBracketId = Widget.joinId myId ["first-bracket"]
    onFirstBracket label =
      ExpressionGui.makeFocusableView firstBracketId label
      & Lens.mapped . ExpressionGui.egWidget %~
        Widget.weakerEvents addFirstElemEventMap
    cursorDest = maybe firstBracketId itemId $ listToMaybe items

makeItem ::
  MonadA m =>
  Sugar.ListItem m (Sugar.ExpressionP m (Sugar.Payload m)) ->
  ExprGuiM m (ExpressionGui m, ExpressionGui m)
makeItem item = do
  (pair, resultPickers) <-
    ExprGuiM.listenResultPickers $
    Lens.sequenceOf Lens.both
    ( ExpressionGui.makeColoredLabel Config.listCommaTextSize
      Config.listCommaColor ", " $ Widget.augmentId ',' itemWidgetId
    , ExprGuiM.makeSubexpresion itemExpr
    )
  return $ pair
    & Lens._2 . ExpressionGui.egWidget %~
    Widget.weakerEvents
    (maybe mempty (mkItemEventMap resultPickers) (Sugar.liMActions item))
  where
    itemExpr = Sugar.liExpr item
    itemWidgetId = WidgetIds.fromGuid $ itemExpr ^. Sugar.rGuid
    mkItemEventMap resultPickers Sugar.ListItemActions
      { Sugar._itemAddNext = addItem
      , Sugar._itemDelete = delItem
      } =
      mconcat
      [ Widget.keysEventMapMovesCursor
        Config.listAddItemKeys (doc resultPickers) $ do
          sequence_ resultPickers
          WidgetIds.fromGuid <$> addItem
      , Widget.keysEventMapMovesCursor
        (Config.delBackwordKeys ++ Config.delForwardKeys)
        (E.Doc ["Edit", "List", "Delete Item"]) $
        WidgetIds.fromGuid <$> delItem
      ]
    doc [] = E.Doc ["Edit", "List", "Add Next Item"]
    doc _ = E.Doc ["Edit", "List", "Pick Result and Add Next Item"]
