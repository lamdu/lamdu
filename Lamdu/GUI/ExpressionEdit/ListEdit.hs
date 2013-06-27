{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ListEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens ((&), (%~), (^.))
import Control.MonadA (MonadA)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Lamdu.GUI.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Sugar.List m (ExprGuiM.SugarExpr m) ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make pl list =
  ExpressionGui.wrapExpression pl $ makeUnwrapped list

makeBracketLabel :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui f)
makeBracketLabel label myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.fromValueWidget <$>
    ExpressionGui.makeColoredLabel
    (Config.listBracketTextSize config)
    (Config.listBracketColor config)
    label myId

makeUnwrapped ::
  MonadA m => Sugar.List m (ExprGuiM.SugarExpr m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.List items mActions) myId =
  ExprGuiM.assignCursor myId cursorDest $
  mapM makeItem items >>= \itemEdits -> do
    bracketOpenLabel <- makeBracketLabel "[" myId
    bracketCloseLabel <- makeBracketLabel "]" myId
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      nilDeleteEventMap =
        actionEventMap (Config.delKeys config) "Replace nil with hole" Sugar.replaceNil
      addFirstElemEventMap =
        actionEventMap (Config.listAddItemKeys config) "Add First Item" Sugar.addFirstItem
      onFirstBracket label =
        ExpressionGui.makeFocusableView firstBracketId label
        & Lens.mapped . ExpressionGui.egWidget %~
          Widget.weakerEvents addFirstElemEventMap
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
    itemId = WidgetIds.fromGuid . (^. Sugar.liExpr . Sugar.rPayload . Sugar.plGuid)
    actionEventMap keys doc actSelect =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "List", doc]) .
       fmap WidgetIds.fromGuid . actSelect) mActions
    firstBracketId = Widget.joinId myId ["first-bracket"]
    cursorDest = maybe firstBracketId itemId $ listToMaybe items

makeItem ::
  MonadA m =>
  Sugar.ListItem m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (ExpressionGui m, ExpressionGui m)
makeItem item = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    mkItemEventMap resultPickers Sugar.ListItemActions
      { Sugar._itemAddNext = addItem
      , Sugar._itemDelete = delItem
      } =
      mconcat
      [ Widget.keysEventMapMovesCursor
        (Config.listAddItemKeys config) (doc resultPickers) $ do
          sequence_ resultPickers
          WidgetIds.fromGuid <$> addItem
      , Widget.keysEventMapMovesCursor (Config.delKeys config)
        (E.Doc ["Edit", "List", "Delete Item"]) $
        WidgetIds.fromGuid <$> delItem
      ]
  (pair, resultPickers) <-
    ExprGuiM.listenResultPickers $
    Lens.sequenceOf Lens.both
    ( fmap ExpressionGui.fromValueWidget .
      ExpressionGui.makeColoredLabel (Config.listCommaTextSize config)
      (Config.listCommaColor config) ", " $ Widget.augmentId ',' itemWidgetId
    , ExprGuiM.makeSubexpression 0 itemExpr
    )
  return $ pair
    & Lens._2 . ExpressionGui.egWidget %~
    Widget.weakerEvents
    (maybe mempty (mkItemEventMap resultPickers) (item ^. Sugar.liMActions))
  where
    itemExpr = item ^. Sugar.liExpr
    itemWidgetId = WidgetIds.fromGuid $ itemExpr ^. Sugar.rPayload . Sugar.plGuid
    doc [] = E.Doc ["Edit", "List", "Add Next Item"]
    doc _ = E.Doc ["Edit", "List", "Pick Result and Add Next Item"]
