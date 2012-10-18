{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit
  ( makeCodeEdit
  , SugarCache(..), makeSugarCache )
where

import Control.Monad (liftM)
import Data.List (intersperse)
import Data.List.Utils (enumerate, insertAt, removeAt)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExprGuiM, WidgetT)
import Editor.CodeEdit.Settings (Settings)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.Layers as Layers
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.Definition m
  , mDelPane :: Maybe (Transaction ViewTag m Guid)
  , mMovePaneDown :: Maybe (Transaction ViewTag m ())
  , mMovePaneUp :: Maybe (Transaction ViewTag m ())
  }

data SugarCache m = SugarCache
  { scPanes :: [SugarPane m]
  , scClipboards :: [Sugar.Expression m]
  }

makeNewDefinitionAction :: Monad m => ExprGuiM m (Transaction ViewTag m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- ExprGuiM.otransaction OT.readCursor
  return $ do
    newDefI <- Anchors.makeDefinition
    Anchors.newPane newDefI
    Anchors.savePreJumpPosition curCursor
    return . FocusDelegator.delegatingId $ WidgetIds.fromIRef newDefI

makeSugarCache :: Monad m => Transaction ViewTag m (SugarCache m)
makeSugarCache = do
  sugarPanes <- makeSugarPanes
  clipboardsP <- Anchors.clipboards
  sugarConfig <- liftM Property.value Anchors.sugarConfig
  clipboardsExprs <-
    mapM (Sugar.loadConvertExpression sugarConfig) $ Property.list clipboardsP
  return SugarCache
    { scPanes = sugarPanes
    , scClipboards = clipboardsExprs
    }

makeSugarPanes :: Monad m => Transaction ViewTag m [SugarPane m]
makeSugarPanes = do
  panes <- Anchors.getP Anchors.panes
  let
    mkMDelPane i
      | length panes >= 1 = Just $ do
        let newPanes = removeAt i panes
        Anchors.setP Anchors.panes newPanes
        return . maybe panesGuid IRef.guid . listToMaybe . reverse $
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
      sugarConfig <- liftM Property.value Anchors.sugarConfig
      sDef <- Sugar.loadConvertDefinition sugarConfig defI
      return SugarPane
        { spDef = sDef
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  mapM convertPane $ enumerate panes

makeClipboardsEdit ::
  MonadF m => [Sugar.Expression m] -> ExprGuiM m (WidgetT m)
makeClipboardsEdit clipboards = do
  clipboardsEdits <-
    mapM (liftM ExpressionGui.egWidget . ExpressionEdit.make) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else ExprGuiM.otransaction $ BWidgets.makeTextView "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

makeCodeEdit ::
  MonadF m =>
  Settings -> SugarCache m -> OTransaction ViewTag m (IT.WidgetT ViewTag m)
makeCodeEdit settings cache =
  ExpressionGui.runExprGuiM ExpressionEdit.make settings $ do
    panesEdit <- makePanesEdit $ scPanes cache
    clipboardsEdit <- makeClipboardsEdit $ scClipboards cache
    return $
      Box.vboxAlign 0
      [ panesEdit
      , clipboardsEdit
      ]

panesGuid :: Guid
panesGuid = IRef.guid Anchors.panesIRef

makePanesEdit :: MonadF m => [SugarPane m] -> ExprGuiM m (WidgetT m)
makePanesEdit panes = do
  panesWidget <-
    case panes of
    [] -> ExprGuiM.otransaction $ BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (ExprGuiM.assignCursor myId . WidgetIds.fromGuid . Sugar.drGuid . spDef) firstPane $ do
        definitionEdits <- mapM makePaneWidget panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  mJumpBack <- ExprGuiM.transaction Anchors.jumpBack
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
    myId = WidgetIds.fromGuid panesGuid
    paneEventMap pane = fmap IT.transaction $ mconcat
      [ maybe mempty (Widget.keysEventMapMovesCursor Config.closePaneKeys "Close pane" . liftM WidgetIds.fromGuid) $ mDelPane pane
      , maybe mempty (Widget.keysEventMap Config.movePaneDownKeys "Move pane down") $ mMovePaneDown pane
      , maybe mempty (Widget.keysEventMap Config.movePaneUpKeys "Move pane up") $ mMovePaneUp pane
      ]
    onEachPane widget
      | Widget.wIsFocused widget = onActivePane widget
      | otherwise = onInactivePane widget
    onActivePane =
      Widget.backgroundColor Layers.activePane WidgetIds.activeDefBackground Config.activeDefBGColor
    onInactivePane =
      (Widget.atWFrame . Anim.onImages . Draw.tint)
      Config.inactiveTintColor
    makePaneWidget pane =
      liftM (onEachPane . Widget.weakerEvents (paneEventMap pane)) .
      makeDefinitionEdit $ spDef pane
    makeDefinitionEdit = DefinitionEdit.make
