{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit (make, Env(..)) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.List (intersperse)
import Data.List.Utils (enumerate, insertAt, removeAt)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Load (loadDef)
import Lamdu.GUI.CodeEdit.Settings (Settings)
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
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Convert as SugarConvert

type T = Transaction

data Pane m = Pane
  { paneDefI :: DefI m
  , paneDel :: Maybe (T m Guid)
  , paneMoveDown :: Maybe (T m ())
  , paneMoveUp :: Maybe (T m ())
  }

data Env m = Env
  { codeProps :: Anchors.CodeProps m
  , totalSize :: Widget.Size
  , settings :: Settings
  }

totalWidth :: Env m -> Widget.R
totalWidth = (^. Lens._1) . totalSize

makePanes :: MonadA m => Transaction.Property m [DefI m] -> Guid -> [Pane m]
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

makeClipboardsEdit :: MonadA m => Env m -> [DefI m] -> WidgetEnvT (T m) (Widget (T m))
makeClipboardsEdit env clipboards = do
  clipboardsEdits <- traverse (makePaneWidget env) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else BWidgets.makeTextViewWidget "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

getClipboards :: MonadA m => Anchors.CodeProps m -> T m [DefI m]
getClipboards = Transaction.getP . Anchors.clipboards

make :: MonadA m => Env m -> Guid -> WidgetEnvT (T m) (Widget (T m))
make env rootGuid = do
  prop <- lift $ Anchors.panes (codeProps env) ^. Transaction.mkProperty
  (sugarPanes, sugarClipboards) <-
    (,) (makePanes prop rootGuid) <$> (lift . getClipboards) (codeProps env)
  panesEdit <- makePanesEdit env sugarPanes $ WidgetIds.fromGuid rootGuid
  clipboardsEdit <- makeClipboardsEdit env sugarClipboards
  return $
    Box.vboxAlign 0
    [ panesEdit
    , clipboardsEdit
    ]

makePanesEdit :: MonadA m => Env m -> [Pane m] -> Widget.Id -> WidgetEnvT (T m) (Widget (T m))
makePanesEdit env panes myId = do
  config <- WE.readConfig
  let
    Config.Pane{..} = Config.pane config
    paneEventMap pane = mconcat
      [ maybe mempty
        (Widget.keysEventMapMovesCursor paneCloseKeys
         (E.Doc ["View", "Pane", "Close"]) . fmap WidgetIds.fromGuid) $ paneDel pane
      , maybe mempty
        (Widget.keysEventMap paneMoveDownKeys
         (E.Doc ["View", "Pane", "Move down"])) $ paneMoveDown pane
      , maybe mempty
        (Widget.keysEventMap paneMoveUpKeys
         (E.Doc ["View", "Pane", "Move up"])) $ paneMoveUp pane
      ]
    makePaneEdit pane =
      (fmap . Widget.weakerEvents) (paneEventMap pane) .
      makePaneWidget env . paneDefI $ pane
  panesWidget <-
    case panes of
    [] -> BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (WE.assignCursor myId . WidgetIds.fromIRef . paneDefI) firstPane $ do
        definitionEdits <- traverse makePaneEdit panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  mJumpBack <- lift . DataOps.jumpBack $ codeProps env
  newDefinition <- DefinitionEdit.makeNewDefinition $ codeProps env
  let
    panesEventMap =
      mconcat
      [ Widget.keysEventMapMovesCursor newDefinitionKeys
        (E.Doc ["Edit", "New definition"]) newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
         (E.Doc ["Navigation", "Go back"])) mJumpBack
      ]
  return $ Widget.weakerEvents panesEventMap panesWidget

makePaneWidget :: MonadA m => Env m -> DefI m -> WidgetEnvT (T m) (Widget (T m))
makePaneWidget env defI = do
  config <- WE.readConfig
  let
    Config.Pane{..} = Config.pane config
    colorize widget
      | widget ^. Widget.wIsFocused = colorizeActivePane widget
      | otherwise = colorizeInactivePane widget
    colorizeActivePane =
      Widget.backgroundColor
      (Config.layerActivePane (Config.layers config))
      WidgetIds.activeDefBackground paneActiveBGColor
    colorizeInactivePane =
      Widget.wFrame %~ Anim.onImages (Draw.tint paneInactiveTintColor)
  loadDef defI
    >>= SugarConvert.convertDefI (codeProps env)
    & lift
    >>= DefinitionEdit.make (codeProps env) (settings env)
    <&> fitToWidth (totalWidth env) . colorize

fitToWidth :: Widget.R -> Widget f -> Widget f
fitToWidth width w
  | ratio < 1 = w & Widget.scale (realToFrac ratio)
  | otherwise = w
  where
    ratio = width / w ^. Widget.wSize . Lens._1
