{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit (make) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT, mapStateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.List (intersperse)
import Data.List.Utils (enumerate, insertAt, removeAt)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (WidgetT, ExprGuiM)
import Lamdu.CodeEdit.Settings (Settings)
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.DefinitionEdit as DefinitionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.CodeEdit.Sugar.AddNames as AddNames
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction
type CT m = StateT Cache (T m)

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.DefinitionU m
  , mDelPane :: Maybe (T m Guid)
  , mMovePaneDown :: Maybe (T m ())
  , mMovePaneUp :: Maybe (T m ())
  }

makeNewDefinitionAction :: MonadA m => ExprGuiM m (T m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- ExprGuiM.widgetEnv WE.readCursor
  cp <- ExprGuiM.readCodeAnchors
  return $ do
    newDefI <- DataOps.newPublicDefinition cp ""
    DataOps.newPane cp newDefI
    DataOps.savePreJumpPosition cp curCursor
    return . DefinitionEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI

makeSugarPanes :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> Guid -> CT m [SugarPane m]
makeSugarPanes cp rootGuid = do
  panes <- lift . Transaction.getP $ Anchors.panes cp
  let
    mkMDelPane i
      | not (null panes) = Just $ do
        let newPanes = removeAt i panes
        Transaction.setP (Anchors.panes cp) newPanes
        return . maybe rootGuid IRef.guid . listToMaybe . reverse $
          take (i+1) newPanes
      | otherwise = Nothing
    movePane oldIndex newIndex = do
      let
        (before, item:after) = splitAt oldIndex panes
        newPanes = insertAt newIndex item $ before ++ after
      Transaction.setP (Anchors.panes cp) newPanes
    mkMMovePaneDown i
      | i+1 < length panes = Just $ movePane i (i+1)
      | otherwise = Nothing
    mkMMovePaneUp i
      | i-1 >= 0 = Just $ movePane i (i-1)
      | otherwise = Nothing
    convertPane (i, defI) = do
      sDef <- Sugar.loadConvertDefI cp defI
      return SugarPane
        { spDef = sDef
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  traverse convertPane $ enumerate panes

makeClipboardsEdit ::
  MonadA m => [Sugar.DefinitionU m] -> ExprGuiM m (WidgetT m)
makeClipboardsEdit clipboards = do
  clipboardsEdits <- traverse makePaneWidget clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else ExprGuiM.widgetEnv $ BWidgets.makeTextView "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

makeSugarClipboards :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> CT m [Sugar.DefinitionU m]
makeSugarClipboards cp =
  traverse (Sugar.loadConvertDefI cp) =<<
  (lift . Transaction.getP . Anchors.clipboards) cp

make ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> Settings -> Guid ->
  StateT Cache (WidgetEnvT (T m)) (Widget (T m))
make cp settings rootGuid = do
  (sugarPanes, sugarClipboards) <-
    mapStateT lift $
    (,) <$> makeSugarPanes cp rootGuid <*> makeSugarClipboards cp
  ExprGuiM.run ExpressionEdit.make cp settings $ do
    panesEdit <- makePanesEdit sugarPanes $ WidgetIds.fromGuid rootGuid
    clipboardsEdit <- makeClipboardsEdit sugarClipboards
    return $
      Box.vboxAlign 0
      [ panesEdit
      , clipboardsEdit
      ]

makePanesEdit :: MonadA m => [SugarPane m] -> Widget.Id -> ExprGuiM m (WidgetT m)
makePanesEdit panes myId = do
  panesWidget <-
    case panes of
    [] -> ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (ExprGuiM.assignCursor myId . WidgetIds.fromGuid . (^. Sugar.drGuid) . spDef) firstPane $ do
        definitionEdits <- traverse makePaneEdit panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  cp <- ExprGuiM.readCodeAnchors
  mJumpBack <- ExprGuiM.transaction $ DataOps.jumpBack cp
  newDefinition <- makeNewDefinitionAction
  let
    panesEventMap =
      mconcat
      [ Widget.keysEventMapMovesCursor Config.newDefinitionKeys
        (E.Doc ["Edit", "New definition"]) newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor Config.previousCursorKeys
         (E.Doc ["Navigation", "Go back"])) mJumpBack
      ]

  return $ Widget.weakerEvents panesEventMap panesWidget
  where
    makePaneEdit pane =
      (fmap . Widget.weakerEvents) (paneEventMap pane) .
      makePaneWidget . spDef $ pane
    paneEventMap pane = mconcat
      [ maybe mempty
        (Widget.keysEventMapMovesCursor Config.closePaneKeys
         (E.Doc ["View", "Pane", "Close"]) . fmap WidgetIds.fromGuid) $ mDelPane pane
      , maybe mempty
        (Widget.keysEventMap Config.movePaneDownKeys
         (E.Doc ["View", "Pane", "Move down"])) $ mMovePaneDown pane
      , maybe mempty
        (Widget.keysEventMap Config.movePaneUpKeys
         (E.Doc ["View", "Pane", "Move up"])) $ mMovePaneUp pane
      ]

makePaneWidget :: MonadA m => Sugar.DefinitionU m -> ExprGuiM m (Widget (T m))
makePaneWidget rawDefS = do
  infoMode <- (^. Settings.sInfoMode) <$> ExprGuiM.readSettings
  let
    defS =
      case infoMode of
      Settings.Types -> rawDefS
      _ -> Sugar.removeTypes <$> rawDefS
  onEachPane <$> DefinitionEdit.make (AddNames.addToDef defS)
  where
    onEachPane widget
      | widget ^. Widget.wIsFocused = onActivePane widget
      | otherwise = onInactivePane widget
    onActivePane =
      Widget.backgroundColor Layers.activePane WidgetIds.activeDefBackground Config.activeDefBGColor
    onInactivePane =
      (Lens.over Widget.wFrame . Anim.onImages . Draw.tint)
      Config.inactiveTintColor
