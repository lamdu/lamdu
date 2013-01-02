{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ListEdit(make, makeEnumFromTo) where

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

makeEnumFromTo ::
  MonadA m => Sugar.EnumFromTo (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeEnumFromTo eft = ExpressionGui.wrapExpression $ makeEnumFromToUnwrapped eft

makeEnumFromToUnwrapped ::
  MonadA m => Sugar.EnumFromTo (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeEnumFromToUnwrapped (Sugar.EnumFromTo lo hi) myId =
  ExprGuiM.assignCursor myId cursorDest $
  ExpressionGui.hbox <$> sequence
    [ makeBracketLabel "[" myId
    , ExprGuiM.makeSubexpresion lo
    , ExpressionGui.makeColoredLabel Config.enumFromToDotSize
      Config.enumFromToDotColor ".." myId
    , ExprGuiM.makeSubexpresion hi
    , makeBracketLabel "]" myId
    ]
  where
    cursorDest = WidgetIds.fromGuid $ lo ^. Sugar.rGuid

make ::
  MonadA m => Sugar.List m (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make list = ExpressionGui.wrapExpression $ makeUnwrapped list

makeBracketLabel :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui f)
makeBracketLabel =
  ExpressionGui.makeColoredLabel Config.listBracketTextSize Config.listBracketColor

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
    bracketClose <- ExpressionGui.makeFocusableView closeBracketId =<< makeBracketLabel "]" myId
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
      (ExpressionGui.makeFocusableView firstBracketId =<< makeBracketLabel txt myId)
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
