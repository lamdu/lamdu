{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Monad (liftM)
import Data.List.Utils(enumerate, insertAt, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, getP, assignCursor, readCursor, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

makeNewDefinitionAction :: Monad m => CTransaction ViewTag m (Transaction.Transaction ViewTag m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- readCursor
  return $ do
    newDefI <- Anchors.makeDefinition ""
    Anchors.newPane newDefI
    Anchors.savePreJumpPosition curCursor
    return $ WidgetIds.fromIRef newDefI

makePanesEdit :: MonadF m => TWidget ViewTag m
makePanesEdit = do
  panes <- getP Anchors.panes

  let
    delPane i = do
      let newPanes = removeAt i panes
      Property.set Anchors.panes newPanes
      return . WidgetIds.fromIRef . last $
        take (i+1) newPanes
    movePane oldIndex newIndex = do
      let
        (before, item:after) = splitAt oldIndex panes
        newPanes = insertAt newIndex item $ before ++ after
      Property.set Anchors.panes newPanes

    paneEventMap (_:_:_) i = mconcat $ concat
      [ [ Widget.actionEventMapMovesCursor Config.closePaneKeys "Close pane" $ delPane i ]
      , [ Widget.actionEventMap Config.movePaneDownKeys "Move pane down" $ movePane i (i+1)
        | i+1 < length panes
        ]
      , [ Widget.actionEventMap Config.movePaneUpKeys "Move pane up" $ movePane i (i-1)
        | i-1 >= 0
        ]
      ]
    paneEventMap _ _ = mempty

    makePaneWidget (i, pane) = do
      def <- transaction $ Sugar.convertDefinition pane
      defEdit <- DefinitionEdit.make ExpressionEdit.make def
      return $ Widget.weakerEvents (paneEventMap panes i) defEdit

  panesWidget <-
    case panes of
      [] -> BWidgets.makeFocusableTextView "<No panes>" myId
      (firstPane:_) ->
        assignCursor myId
          (WidgetIds.fromIRef firstPane) $ do
            definitionEdits <- mapM makePaneWidget $ enumerate panes
            return $ BWidgets.vboxAlign 0 definitionEdits

  canJumpBack <- transaction Anchors.canJumpBack
  newDefinition <- makeNewDefinitionAction
  let
    panesEventMap =
      mconcat . concat $
      [[ Widget.actionEventMapMovesCursor Config.newDefinitionKeys
         "New definition" newDefinition
       ]
      ,[ Widget.actionEventMapMovesCursor Config.previousCursorKeys
         "Go to previous position" $ liftM (fromMaybe myId) Anchors.jumpBack
       | canJumpBack
       ]
      ]

  return $ Widget.weakerEvents panesEventMap panesWidget
  where
    myId = WidgetIds.fromIRef Anchors.panesIRef
