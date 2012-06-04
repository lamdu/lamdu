{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit
  ( makeCodeEdit
  , SugarCache(..), makeSugarCache )
where

import Control.Monad (liftM, (<=<))
import Data.List.Utils(enumerate, insertAt, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, assignCursor, readCursor, transaction)
import Editor.MonadF (MonadF)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data.Typed as DataTyped
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.DefinitionRef m
  , mDelPane :: Maybe (Transaction ViewTag m Guid)
  , mMovePaneDown :: Maybe (Transaction ViewTag m ())
  , mMovePaneUp :: Maybe (Transaction ViewTag m ())
  }

data SugarCache m = SugarCache
  { scPanes :: [SugarPane m]
  , scClipboards :: [Sugar.ExpressionRef m]
  }

makeNewDefinitionAction :: Monad m => CTransaction ViewTag m (Transaction ViewTag m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- readCursor
  return $ do
    newDefI <- Anchors.makeDefinition ""
    Anchors.newPane newDefI
    Anchors.savePreJumpPosition curCursor
    return $ WidgetIds.fromIRef newDefI

makeSugarCache :: Monad m => Transaction ViewTag m (SugarCache m)
makeSugarCache = do
  sugarPanes <- makeSugarPanes
  clipboards <- Property.get Anchors.clipboards
  clipboardsExprs <- mapM (Sugar.convertExpression <=< DataTyped.loadInferExpression) clipboards
  return SugarCache
    { scPanes = sugarPanes
    , scClipboards = clipboardsExprs
    }

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
      typedDef <- DataTyped.loadInferDefinition defI
      def <- Sugar.convertDefinition typedDef
      return SugarPane
        { spDef = def
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  mapM convertPane $ enumerate panes

makeClipboardsEdit :: MonadF m => [Sugar.ExpressionRef m] -> TWidget ViewTag m
makeClipboardsEdit clipboards = do
  clipboardsEdits <- mapM ExpressionEdit.make clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return BWidgets.empty
    else BWidgets.makeTextView "Clipboards:" ["clipboards title"]
  return .
    BWidgets.vbox . (clipboardTitle :) . concat $ zipWith addLineBefore [0..] clipboardsEdits
  where
    addLineBefore i clipboardEdit =
      [ Spacer.makeHorizLineWidget ["clipboard line:", BS8.pack (show (i :: Int))]
      , clipboardEdit
      ]


makeCodeEdit :: MonadF m => SugarCache m -> TWidget ViewTag m
makeCodeEdit cache = do
  panesEdit <- makePanesEdit $ scPanes cache
  clipboardsEdit <- makeClipboardsEdit $ scClipboards cache
  return $
    BWidgets.vbox
    [ panesEdit
    , Widget.liftView Spacer.makeVerticalExpanding
    , clipboardsEdit]

makePanesEdit :: MonadF m => [SugarPane m] -> TWidget ViewTag m
makePanesEdit panes = do
  panesWidget <-
    case panes of
    [] -> BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (assignCursor myId . WidgetIds.fromGuid . Sugar.drGuid . spDef) firstPane $ do
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
      makeDefinitionEdit $ spDef pane
    makeDefinitionEdit (Sugar.DefinitionRef guid defBody defType inferredTypes) =
      DefinitionEdit.make ExpressionEdit.make guid defBody defType inferredTypes
