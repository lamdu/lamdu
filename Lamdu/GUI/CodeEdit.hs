{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit (make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, mapStateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.List (intersperse)
import Data.List.Utils (enumerate, insertAt, removeAt)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (WidgetT, ExprGuiM)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds

type T = Transaction
type CT m = StateT Cache (T m)

data Pane m = Pane
  { paneDefI :: DefIM m
  , paneDel :: Maybe (T m Guid)
  , paneMoveDown :: Maybe (T m ())
  , paneMoveUp :: Maybe (T m ())
  }

makePanes ::
  (MonadA m, Typeable1 m) =>
  Transaction.Property m [DefIM m] -> Guid -> [Pane m]
makePanes (Property panes setPanes) rootGuid =
  convertPane <$> enumerate panes
  where
    mkMDelPane i
      | not (null panes) = Just $ do
        let newPanes = removeAt i panes
        setPanes newPanes
        return . maybe rootGuid IRef.guid . listToMaybe . reverse $
          take (i+1) newPanes
      | otherwise = Nothing
    movePane oldIndex newIndex = do
      let
        (before, item:after) = splitAt oldIndex panes
        newPanes = insertAt newIndex item $ before ++ after
      setPanes newPanes
    mkMMovePaneDown i
      | i+1 < length panes = Just $ movePane i (i+1)
      | otherwise = Nothing
    mkMMovePaneUp i
      | i-1 >= 0 = Just $ movePane i (i-1)
      | otherwise = Nothing
    convertPane (i, defI) = Pane
      { paneDefI = defI
      , paneDel = mkMDelPane i
      , paneMoveDown = mkMMovePaneDown i
      , paneMoveUp = mkMMovePaneUp i
      }

makeClipboardsEdit ::
  (Typeable1 m, MonadA m) => Widget.R -> [DefIM m] -> ExprGuiM m (WidgetT m)
makeClipboardsEdit width clipboards = do
  clipboardsEdits <- traverse (makePaneWidget width) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else ExprGuiM.widgetEnv $ BWidgets.makeTextViewWidget "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

getClipboards :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> CT m [DefIM m]
getClipboards = lift . Transaction.getP . Anchors.clipboards

make ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> Widget.Size -> Settings -> Guid ->
  StateT Cache (WidgetEnvT (T m)) (Widget (T m))
make cp size settings rootGuid = do
  prop <- lift . lift $ Anchors.panes cp ^. Transaction.mkProperty
  (sugarPanes, sugarClipboards) <-
    mapStateT lift $
    (,) (makePanes prop rootGuid) <$> getClipboards cp
  ExprGuiM.run ExpressionEdit.make cp settings $ do
    panesEdit <- makePanesEdit width sugarPanes $ WidgetIds.fromGuid rootGuid
    clipboardsEdit <- makeClipboardsEdit width sugarClipboards
    return $
      Box.vboxAlign 0
      [ panesEdit
      , clipboardsEdit
      ]
  where
    width = size ^. Lens._1

makePanesEdit ::
  (Typeable1 m, MonadA m) => Widget.R -> [Pane m] -> Widget.Id -> ExprGuiM m (WidgetT m)
makePanesEdit width panes myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    paneEventMap pane = mconcat
      [ maybe mempty
        (Widget.keysEventMapMovesCursor (Config.closePaneKeys config)
         (E.Doc ["View", "Pane", "Close"]) . fmap WidgetIds.fromGuid) $ paneDel pane
      , maybe mempty
        (Widget.keysEventMap (Config.movePaneDownKeys config)
         (E.Doc ["View", "Pane", "Move down"])) $ paneMoveDown pane
      , maybe mempty
        (Widget.keysEventMap (Config.movePaneUpKeys config)
         (E.Doc ["View", "Pane", "Move up"])) $ paneMoveUp pane
      ]
    makePaneEdit pane =
      (fmap . Widget.weakerEvents) (paneEventMap pane) .
      makePaneWidget width . paneDefI $ pane
  panesWidget <-
    case panes of
    [] -> ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (ExprGuiM.assignCursor myId . WidgetIds.fromIRef . paneDefI) firstPane $ do
        definitionEdits <- traverse makePaneEdit panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  cp <- ExprGuiM.readCodeAnchors
  mJumpBack <- ExprGuiM.transaction $ DataOps.jumpBack cp
  newDefinition <- DefinitionEdit.makeNewDefinition
  let
    panesEventMap =
      mconcat
      [ Widget.keysEventMapMovesCursor (Config.newDefinitionKeys config)
        (E.Doc ["Edit", "New definition"]) newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
         (E.Doc ["Navigation", "Go back"])) mJumpBack
      ]
  return $ Widget.weakerEvents panesEventMap panesWidget

makePaneWidget ::
  (Typeable1 m, MonadA m) =>
  Widget.R -> DefIM m -> ExprGuiM m (Widget (T m))
makePaneWidget width defI = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    colorize widget
      | widget ^. Widget.wIsFocused = colorizeActivePane widget
      | otherwise = colorizeInactivePane widget
    colorizeActivePane =
      Widget.backgroundColor
      (Config.layerActivePane (Config.layers config))
      WidgetIds.activeDefBackground $ Config.activeDefBGColor config
    colorizeInactivePane =
      Widget.wFrame %~
      Anim.onImages (Draw.tint (Config.inactiveTintColor config))
  fitToWidth width . colorize <$> DefinitionEdit.make defI

fitToWidth :: Widget.R -> Widget f -> Widget f
fitToWidth width w
  | ratio < 1 = w & Widget.scale (realToFrac ratio)
  | otherwise = w
  where
    ratio = width / w ^. Widget.wSize . Lens._1
