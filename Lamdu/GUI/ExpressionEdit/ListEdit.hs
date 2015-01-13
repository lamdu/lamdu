{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ListEdit(make) where

import           Control.Applicative ((<$>), (<$), Applicative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Monoid (Monoid(..), (<>))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, holePickersAction)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.List m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make list pl =
  ExpressionGui.stdWrapParentExpr pl $ makeUnwrapped list

lastLens :: Lens.Traversal' [a] a
lastLens = Lens.taking 1 . Lens.backwards $ Lens.traversed

makeUnwrapped ::
  MonadA m =>
  Sugar.List m (ExprGuiM.SugarExpr m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped list myId =
  ExprGuiM.assignCursor myId cursorDest $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    bracketOpenLabel <-
      ExpressionGui.grammarLabel "[" (Widget.toAnimId bracketsId)
      >>= ExpressionGui.makeFocusableView firstBracketId
      <&>
        ExpressionGui.egWidget %~
        Widget.weakerEvents
        (actionEventMap (Config.listAddItemKeys config) "Add First Item" Sugar.addFirstItem)
    bracketCloseLabel <- ExpressionGui.grammarLabel "]" (Widget.toAnimId bracketsId)
    case ExpressionGui.listWithDelDests firstBracketId firstBracketId itemId (Sugar.lValues list) of
      [] ->
        return $ ExpressionGui.hbox [bracketOpenLabel, bracketCloseLabel]
      firstValue : nextValues -> do
        (_, firstEdit) <- makeItem firstValue
        nextEdits <- mapM makeItem nextValues

        jumpHolesEventMap <-
          firstValue ^. _3 . Sugar.liExpr
          & ExprGuiM.nextHolesBefore
          & ExprEventMap.jumpHolesEventMap []
        let
          nilDeleteEventMap =
            actionEventMap (Config.delKeys config) "Replace nil with hole" Sugar.replaceNil
          addLastEventMap =
            maybe mempty
            ( Widget.keysEventMapMovesCursor (Config.listAddItemKeys config)
              (E.Doc ["Edit", "List", "Add Last Item"])
            . fmap WidgetIds.fromEntityId
            ) $ Sugar.lValues list ^? lastLens . Sugar.liMActions . Lens._Just . Sugar.itemAddNext
          closerEventMap = mappend nilDeleteEventMap addLastEventMap
        bracketClose <-
          ExpressionGui.makeFocusableView closeBracketId bracketCloseLabel
          <&> ExpressionGui.egWidget %~ Widget.weakerEvents closerEventMap
        return . ExpressionGui.hbox $ concat
          [ [ bracketOpenLabel
              & ExpressionGui.egWidget %~ Widget.weakerEvents jumpHolesEventMap
            , firstEdit
            ]
          , nextEdits >>= pairToList
          , [ bracketClose ]
          ]
  where
    bracketsId = WidgetIds.fromEntityId $ Sugar.lNilEntityId list
    pairToList (x, y) = [x, y]
    closeBracketId = Widget.joinId myId ["close-bracket"]
    itemId = WidgetIds.fromEntityId . (^. Sugar.liExpr . Sugar.rPayload . Sugar.plEntityId)
    actionEventMap keys doc actSelect =
      maybe mempty
      ( Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "List", doc])
      . fmap WidgetIds.fromEntityId . actSelect ) $
      Sugar.lMActions list
    firstBracketId = mappend (Widget.Id ["first bracket"]) bracketsId
    cursorDest = maybe firstBracketId itemId $ Sugar.lValues list ^? Lens.traversed

makeItem ::
  MonadA m =>
  (Widget.Id, Widget.Id, Sugar.ListItem m (ExprGuiM.SugarExpr m)) ->
  ExprGuiM m (ExpressionGui m, ExpressionGui m)
makeItem (_, nextId, item) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    mkItemEventMap resultPickers Sugar.ListItemActions
      { Sugar._itemAddNext = addItem
      , Sugar._itemDelete = delItem
      } =
      mconcat
      [ E.keyPresses (Config.listAddItemKeys config) (doc resultPickers) $
        mappend
        <$> holePickersAction resultPickers
        <*> (Widget.eventResultFromCursor . WidgetIds.fromEntityId <$> addItem)
      , Widget.keysEventMapMovesCursor (Config.delKeys config)
        (E.Doc ["Edit", "List", "Delete Item"]) $
        nextId <$ delItem
      ]
  (pair, resultPickers) <-
    ExprGuiM.listenResultPickers $
    Lens.sequenceOf Lens.both
    ( ExpressionGui.grammarLabel ", " (Widget.toAnimId itemWidgetId <> [","])
    , ExprGuiM.makeSubexpression 0 itemExpr
    )
  return $ pair
    & _2 . ExpressionGui.egWidget %~
    Widget.weakerEvents
    (maybe mempty (mkItemEventMap resultPickers) (item ^. Sugar.liMActions))
  where
    itemExpr = item ^. Sugar.liExpr
    itemWidgetId = WidgetIds.fromEntityId $ itemExpr ^. Sugar.rPayload . Sugar.plEntityId
    doc [] = E.Doc ["Edit", "List", "Add Next Item"]
    doc _ = E.Doc ["Edit", "List", "Pick Result and Add Next Item"]
