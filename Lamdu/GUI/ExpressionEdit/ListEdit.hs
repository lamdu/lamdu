{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ListEdit(make) where

import Control.Applicative ((<$>), (<|>), Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, holePickersAction)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.List m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make list pl =
  ExpressionGui.stdWrapParentExpr pl $ makeUnwrapped pl list

makeBracketLabel :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui f)
makeBracketLabel label myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.fromValueWidget <$>
    ExpressionGui.makeColoredLabel
    (Config.listBracketTextSize config)
    (Config.listBracketColor config)
    label myId

lastLens :: Lens.Traversal' [a] a
lastLens = Lens.taking 1 . Lens.backwards $ Lens.traversed

makeUnwrapped ::
  MonadA m =>
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Sugar.List m (ExprGuiM.SugarExpr m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped pl list myId =
  ExprGuiM.assignCursor myId cursorDest $ do
    bracketOpenLabel <- makeBracketLabel "[" bracketsIdForAnim
    bracketCloseLabel <- makeBracketLabel "]" bracketsIdForAnim
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      addFirstElemEventMap =
        actionEventMap (Config.listAddItemKeys config) "Add First Item" Sugar.addFirstItem
      onFirstBracket mItem itemPl label = do
        let hg = itemPl ^. Sugar.plData . ExprGuiM.plHoleGuids
        jumpHolesEventMap <-
          hg
          & case mItem of
            Just item
              | Lens.has (Sugar.liExpr . Sugar.rBody . Sugar._BodyHole) item
              -> ExprGuiM.hgMNextHole %~ (storedGuid <|>)
              where
                storedGuid =
                  item ^? Sugar.liExpr . Sugar.rPayload .
                  Sugar.plActions . Lens._Just . Sugar.storedGuid
            _ -> id
          & ExprEventMap.jumpHolesEventMap []
        ExpressionGui.makeFocusableView firstBracketId label
          <&> ExpressionGui.egWidget %~
              Widget.weakerEvents (mappend addFirstElemEventMap jumpHolesEventMap)
    case Sugar.lValues list of
      [] -> onFirstBracket Nothing pl $ ExpressionGui.hbox [bracketOpenLabel, bracketCloseLabel]
      firstValue : nextValues -> do
        (_, firstEdit) <- makeItem firstValue
        nextEdits <- mapM makeItem nextValues
        bracketOpen <-
          onFirstBracket (Just firstValue) (firstValue ^. Sugar.liExpr . Sugar.rPayload) bracketOpenLabel
        let
          nilDeleteEventMap =
            actionEventMap (Config.delKeys config) "Replace nil with hole" Sugar.replaceNil
          addLastEventMap =
            maybe mempty
            ( Widget.keysEventMapMovesCursor (Config.listAddItemKeys config)
              (E.Doc ["Edit", "List", "Add Last Item"])
            . fmap WidgetIds.fromGuid
            ) $ Sugar.lValues list ^? lastLens . Sugar.liMActions . Lens._Just . Sugar.itemAddNext
          closerEventMap = mappend nilDeleteEventMap addLastEventMap
        bracketClose <-
          ExpressionGui.makeFocusableView closeBracketId bracketCloseLabel
          <&> ExpressionGui.egWidget %~ Widget.weakerEvents closerEventMap
        return . ExpressionGui.hbox $ concat
          [[bracketOpen, firstEdit], nextEdits >>= pairToList, [bracketClose]]
  where
    bracketsIdForAnim = WidgetIds.fromGuid $ Sugar.lNilGuid list
    pairToList (x, y) = [x, y]
    closeBracketId = Widget.joinId myId ["close-bracket"]
    itemId = WidgetIds.fromGuid . (^. Sugar.liExpr . Sugar.rPayload . Sugar.plGuid)
    actionEventMap keys doc actSelect =
      maybe mempty
      ( Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "List", doc])
      . fmap WidgetIds.fromGuid . actSelect) $
      Sugar.lMActions list
    firstBracketId = Widget.joinId myId ["first-bracket"]
    cursorDest = maybe firstBracketId itemId $ Sugar.lValues list ^? Lens.traversed

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
      [ E.keyPresses (Config.listAddItemKeys config) (doc resultPickers) $
        mappend
        <$> holePickersAction resultPickers
        <*> (Widget.eventResultFromCursor . WidgetIds.fromGuid <$> addItem)
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
