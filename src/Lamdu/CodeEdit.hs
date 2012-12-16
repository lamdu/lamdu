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
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (WidgetT, ExprGuiM)
import Lamdu.CodeEdit.Settings (Settings)
import Lamdu.Data.IRef (DefI)
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.DefinitionEdit as DefinitionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction
type CT m = StateT Cache (T m)

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.Definition m
  , mDelPane :: Maybe (T m Guid)
  , mMovePaneDown :: Maybe (T m ())
  , mMovePaneUp :: Maybe (T m ())
  }

makeNewDefinitionAction :: m ~ ViewM => ExprGuiM m (T m Widget.Id)
makeNewDefinitionAction = do
  curCursor <- ExprGuiM.widgetEnv WE.readCursor
  return $ do
    newDefI <- DataOps.makeDefinition
    DataOps.newPane newDefI
    DataOps.savePreJumpPosition curCursor
    return . DefinitionEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI

loadConvertDefI :: DefI (Tag ViewM) -> CT ViewM (Sugar.Definition ViewM)
loadConvertDefI defI = do
  sugarConfig <- lift $ Anchors.getP Anchors.sugarConfig
  Sugar.loadConvertDefI sugarConfig defI

makeSugarPanes :: m ~ ViewM => CT m [SugarPane m]
makeSugarPanes = do
  panes <- lift $ Anchors.getP Anchors.panes
  let
    mkMDelPane i
      | not (null panes) = Just $ do
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
      sDef <- loadConvertDefI defI
      return SugarPane
        { spDef = sDef
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  traverse convertPane $ enumerate panes

makeClipboardsEdit ::
  m ~ ViewM => [Sugar.Definition m] -> ExprGuiM m (WidgetT m)
makeClipboardsEdit clipboards = do
  clipboardsEdits <- traverse makePaneWidget clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else ExprGuiM.widgetEnv $ BWidgets.makeTextView "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

makeSugarClipboards :: CT ViewM [Sugar.Definition ViewM]
makeSugarClipboards =
  traverse loadConvertDefI =<< lift (Anchors.getP Anchors.clipboards)

make ::
  m ~ ViewM => Settings ->
  StateT Cache (WidgetEnvT (T m)) (Widget (T m))
make settings = do
  (sugarPanes, sugarClipboards) <-
    mapStateT lift $
    (,) <$> makeSugarPanes <*> makeSugarClipboards
  ExprGuiM.run ExpressionEdit.make settings $ do
    panesEdit <- makePanesEdit sugarPanes
    clipboardsEdit <- makeClipboardsEdit sugarClipboards
    return $
      Box.vboxAlign 0
      [ panesEdit
      , clipboardsEdit
      ]

panesGuid :: Guid
panesGuid = IRef.guid Anchors.panesIRef

makePanesEdit :: m ~ ViewM => [SugarPane m] -> ExprGuiM m (WidgetT m)
makePanesEdit panes = do
  panesWidget <-
    case panes of
    [] -> ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "<No panes>" myId
    (firstPane:_) ->
      (ExprGuiM.assignCursor myId . WidgetIds.fromGuid . Sugar.drGuid . spDef) firstPane $ do
        definitionEdits <- traverse makePaneEdit panes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits

  mJumpBack <- ExprGuiM.transaction DataOps.jumpBack
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
    myId = WidgetIds.fromGuid panesGuid
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

makePaneWidget :: MonadA m => Sugar.Definition m -> ExprGuiM m (Widget (T m))
makePaneWidget =
  fmap onEachPane . DefinitionEdit.make
  where
    onEachPane widget
      | widget ^. Widget.wIsFocused = onActivePane widget
      | otherwise = onInactivePane widget
    onActivePane =
      Widget.backgroundColor Layers.activePane WidgetIds.activeDefBackground Config.activeDefBGColor
    onInactivePane =
      (Lens.over Widget.wFrame . Anim.onImages . Draw.tint)
      Config.inactiveTintColor
