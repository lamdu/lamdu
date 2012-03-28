{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Monad (liftM)
import Data.List.Utils(enumerate, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

newDefinition :: Monad m => Transaction.Transaction ViewTag m Widget.Id
newDefinition = do
  newDefI <- Anchors.makeDefinition
  Anchors.newPane newDefI
  return $ WidgetIds.fromIRef newDefI

makePanesEdit :: MonadF m => TWidget ViewTag m
makePanesEdit = do
  panes <- getP panesRef

  let
    delPane i = do
      let newPanes = removeAt i panes
      Property.set panesRef newPanes
      return . WidgetIds.fromIRef . Anchors.paneDefinition . last $
        take (i+1) newPanes

    paneEventMap (_:_:_) i =
      Widget.actionEventMapMovesCursor Config.closePaneKeys
        "Close pane" $ delPane i
    paneEventMap _ _ = mempty

    makePaneWidget (i, pane) =
      (liftM . Widget.weakerEvents) (paneEventMap panes i) .
      DefinitionEdit.make ExpressionEdit.make $ Anchors.paneDefinition pane

  panesWidget <-
    case panes of
      [] -> BWidgets.makeFocusableTextView "<No panes>" myId
      (firstPane:_) ->
        assignCursor myId
          (WidgetIds.fromIRef (Anchors.paneDefinition firstPane)) $ do
            definitionEdits <- mapM makePaneWidget $ enumerate panes
            return $ BWidgets.vboxAlign 0 definitionEdits

  canJumpBack <- transaction Anchors.canJumpBack
  let
    panesEventMap =
      mconcat . concat $
      [[ Widget.actionEventMapMovesCursor Config.newDefinitionKeys
        "New definition" newDefinition],
       [ Widget.actionEventMapMovesCursor Config.previousCursorKeys
         "Go to previous position" $ liftM (fromMaybe myId) Anchors.jumpBack
       | canJumpBack]
      ]

  return $ Widget.weakerEvents panesEventMap panesWidget
  where
    panesRef = Transaction.fromIRef Anchors.rootIRef
    myId = WidgetIds.fromIRef Anchors.rootIRef
