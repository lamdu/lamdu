{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit
  ( makePanesEdit
  , SugarPane, makeSugarPanes )
where

import Control.Monad (liftM)
import Data.List.Utils(enumerate, insertAt, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, assignCursor, readCursor, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.DefinitionRef m
  , mDelPane :: Maybe (Transaction ViewTag m Guid)
  , mMovePaneDown :: Maybe (Transaction ViewTag m ())
  , mMovePaneUp :: Maybe (Transaction ViewTag m ())
  }

makeNewDefinitionAction :: Monad m => CTransaction ViewTag m (Transaction ViewTag m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- readCursor
  return $ do
    newDefI <- Anchors.makeDefinition ""
    Anchors.newPane newDefI
    Anchors.savePreJumpPosition curCursor
    return $ WidgetIds.fromIRef newDefI

makeSugarPanes :: Monad m => Transaction ViewTag m [SugarPane m]
makeSugarPanes = do
  panes <- Property.get Anchors.panes
  let
    mkMDelPane i
      | length panes > 1 = Just $ do
        let newPanes = removeAt i panes
        Property.set Anchors.panes newPanes
        return . IRef.guid . last $
          take (i+1) newPanes
      | otherwise = Nothing
    movePane oldIndex newIndex = do
      let
        (before, item:after) = splitAt oldIndex panes
        newPanes = insertAt newIndex item $ before ++ after
      Property.set Anchors.panes newPanes
    mkMMovePaneDown i
      | i+1 < length panes = Just $ movePane i (i+1)
      | otherwise = Nothing
    mkMMovePaneUp i
      | i-1 >= 0 = Just $ movePane i (i-1)
      | otherwise = Nothing
    convertPane (i, defI) = do
      def <- Sugar.loadConvertDefinition defI
      return SugarPane
        { spDef = def
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  mapM convertPane $ enumerate panes

makePanesEdit :: MonadF m => [SugarPane m] -> TWidget ViewTag m
makePanesEdit panes = do
  panesWidget <-
    case panes of
    [] -> BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (assignCursor myId . WidgetIds.fromGuid . Sugar.defGuid . Sugar.drActions . spDef) firstPane $ do
        definitionEdits <- mapM makePaneWidget panes
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
    paneEventMap pane = mconcat
      [ maybe mempty (Widget.actionEventMapMovesCursor Config.closePaneKeys "Close pane" . liftM WidgetIds.fromGuid) $ mDelPane pane
      , maybe mempty (Widget.actionEventMap Config.movePaneDownKeys "Move pane down") $ mMovePaneDown pane
      , maybe mempty (Widget.actionEventMap Config.movePaneUpKeys "Move pane up") $ mMovePaneUp pane
      ]
    makePaneWidget pane =
      liftM (Widget.weakerEvents (paneEventMap pane)) .
      DefinitionEdit.make ExpressionEdit.make $ spDef pane
