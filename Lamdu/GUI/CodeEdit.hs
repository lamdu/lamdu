{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit (make, Env(..)) where

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
import Lamdu.Data.Expr.IRef (DefIM)
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

type T = Transaction
type CT m = StateT Cache (WidgetEnvT (T m))

data Pane m = Pane
  { paneDefI :: DefIM m
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
  (Typeable1 m, MonadA m) => Env m -> [DefIM m] -> CT m (Widget (T m))
makeClipboardsEdit env clipboards = do
  clipboardsEdits <- traverse (makePaneWidget env) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else lift $ BWidgets.makeTextViewWidget "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

getClipboards :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> T m [DefIM m]
getClipboards = Transaction.getP . Anchors.clipboards

make ::
  (MonadA m, Typeable1 m) =>
  Env m -> Guid -> CT m (Widget (T m))
make env rootGuid = do
  prop <- lift . lift $ Anchors.panes (codeProps env) ^. Transaction.mkProperty
  (sugarPanes, sugarClipboards) <-
    (,) (makePanes prop rootGuid) <$> (lift . lift . getClipboards) (codeProps env)
  panesEdit <- makePanesEdit env sugarPanes $ WidgetIds.fromGuid rootGuid
  clipboardsEdit <- makeClipboardsEdit env sugarClipboards
  return $
    Box.vboxAlign 0
    [ panesEdit
    , clipboardsEdit
    ]

makePanesEdit ::
  (Typeable1 m, MonadA m) =>
  Env m -> [Pane m] -> Widget.Id -> CT m (Widget (T m))
makePanesEdit env panes myId = do
  config <- lift WE.readConfig
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
      makePaneWidget env . paneDefI $ pane
  panesWidget <-
    case panes of
    [] -> lift $ BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (mapStateT . WE.assignCursor myId . WidgetIds.fromIRef . paneDefI) firstPane $ do
        definitionEdits <- traverse makePaneEdit panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  mJumpBack <- lift . lift . DataOps.jumpBack $ codeProps env
  newDefinition <- DefinitionEdit.makeNewDefinition $ codeProps env
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
  Env m -> DefIM m -> CT m (Widget (T m))
makePaneWidget env defI = do
  config <- lift WE.readConfig
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
  fitToWidth (totalWidth env) . colorize <$>
    DefinitionEdit.make (codeProps env) (settings env) defI

fitToWidth :: Widget.R -> Widget f -> Widget f
fitToWidth width w
  | ratio < 1 = w & Widget.scale (realToFrac ratio)
  | otherwise = w
  where
    ratio = width / w ^. Widget.wSize . Lens._1
