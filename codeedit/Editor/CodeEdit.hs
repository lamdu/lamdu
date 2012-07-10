{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit
  ( makeCodeEdit
  , SugarCache(..), makeSugarCache )
where

import Control.Monad (liftM, (<=<))
import Data.List.Utils(enumerate, insertAt, removeAt)
import Data.Monoid(Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data.Typed as DataTyped
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

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

makeNewDefinitionAction :: Monad m => OTransaction ViewTag m (Transaction ViewTag m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- OT.readCursor
  return $ do
    newDefI <- Anchors.makeDefinition
    Anchors.newPane newDefI
    Anchors.savePreJumpPosition curCursor
    return $ WidgetIds.fromIRef newDefI

makeSugarCache :: Monad m => Transaction ViewTag m (SugarCache m)
makeSugarCache = do
  sugarPanes <- makeSugarPanes
  clipboardsP <- Anchors.clipboards
  clipboardsExprs <- mapM (Sugar.convertExpression <=< DataTyped.loadInferExpression) $ Property.list clipboardsP
  return SugarCache
    { scPanes = sugarPanes
    , scClipboards = clipboardsExprs
    }

makeSugarPanes :: Monad m => Transaction ViewTag m [SugarPane m]
makeSugarPanes = do
  panes <- Anchors.getP Anchors.panes
  let
    mkMDelPane i
      | length panes > 1 = Just $ do
        let newPanes = removeAt i panes
        Anchors.setP Anchors.panes newPanes
        return . IRef.guid . last $
          take (i+1) newPanes
      | otherwise = Nothing
    movePane oldIndex newIndex = do
      let
        (before, item:after) = splitAt oldIndex panes
        newPanes = insertAt newIndex item $ before ++ after
      Anchors.setP Anchors.panes newPanes
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
  clipboardsEdits <- mapM (liftM ExpressionGui.egWidget . ExpressionEdit.make) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return BWidgets.empty
    else BWidgets.makeTextView "Clipboards:" ["clipboards title"]
  return . BWidgets.vboxAlign 0 $ clipboardTitle : clipboardsEdits

makeCodeEdit :: MonadF m => SugarCache m -> TWidget ViewTag m
makeCodeEdit cache = do
  panesEdit <- makePanesEdit $ scPanes cache
  clipboardsEdit <- makeClipboardsEdit $ scClipboards cache
  return $
    BWidgets.vboxAlign 0
    [ panesEdit
    , clipboardsEdit]

makePanesEdit :: MonadF m => [SugarPane m] -> TWidget ViewTag m
makePanesEdit panes = do
  panesWidget <-
    case panes of
    [] -> BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (OT.assignCursor myId . WidgetIds.fromGuid . Sugar.drGuid . spDef) firstPane $ do
        definitionEdits <- mapM makePaneWidget panes
        return $ BWidgets.vboxAlign 0 definitionEdits

  mJumpBack <- OT.transaction Anchors.jumpBack
  newDefinition <- makeNewDefinitionAction
  let
    panesEventMap =
      fmap IT.transaction $
      mconcat
      [ Widget.keysEventMapMovesCursor Config.newDefinitionKeys
        "New definition" newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor Config.previousCursorKeys
         "Go to previous position") mJumpBack
      ]

  return $ Widget.weakerEvents panesEventMap panesWidget
  where
    myId = WidgetIds.fromIRef Anchors.panesIRef
    paneEventMap pane = fmap IT.transaction $ mconcat
      [ maybe mempty (Widget.keysEventMapMovesCursor Config.closePaneKeys "Close pane" . liftM WidgetIds.fromGuid) $ mDelPane pane
      , maybe mempty (Widget.keysEventMap Config.movePaneDownKeys "Move pane down") $ mMovePaneDown pane
      , maybe mempty (Widget.keysEventMap Config.movePaneUpKeys "Move pane up") $ mMovePaneUp pane
      ]
    onEachPane widget
      | Widget.wIsFocused widget = onActivePane widget
      | otherwise = onInactivePane widget
    onActivePane =
      Widget.backgroundColor 20 WidgetIds.activeDefBackground Config.activeDefBGColor
    onInactivePane =
      (Widget.atWFrame . Anim.onImages . Draw.tint)
      Config.inactiveTintColor
    makePaneWidget pane =
      liftM (onEachPane . Widget.weakerEvents (paneEventMap pane)) .
      makeDefinitionEdit $ spDef pane
    makeDefinitionEdit (Sugar.DefinitionRef guid defBody defType _inferredTypes) =
      DefinitionEdit.make ExpressionEdit.make guid defBody defType
