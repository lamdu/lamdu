{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit (make) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
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
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.AddNextHoles as AddNextHoles
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.AddNames as AddNames
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction
type CT m = StateT Cache (T m)

-- This is not in Sugar because Sugar is for code
data SugarPane m = SugarPane
  { spDef :: Sugar.DefinitionN m ExprGuiM.Payload
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

loadConvertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefIM m ->
  CT m (Sugar.DefinitionN m ExprGuiM.Payload)
loadConvertDefI cp defI =
  lift (Load.loadDefinitionClosure defI) >>=
  SugarConvert.convertDefI cp
  <&> AddNames.addToDef
  <&> Lens.mapped . Lens.mapped . Lens.mapped %~ mkPayload
  <&> AddNextHoles.addToDef
  where
    mkPayload guids = ExprGuiM.Payload
      { ExprGuiM._plStoredGuids = guids
      , ExprGuiM._plInjected = [False]
      , ExprGuiM._plMNextHoleGuid = Nothing -- Filled by AddNextHoles above
      }

makeSugarPanes :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> Guid -> CT m [SugarPane m]
makeSugarPanes cp rootGuid = do
  Property panes setPanes <- lift $ Anchors.panes cp ^. Transaction.mkProperty
  let
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
    convertPane (i, defI) = do
      sDef <- loadConvertDefI cp defI
      return SugarPane
        { spDef = sDef
        , mDelPane = mkMDelPane i
        , mMovePaneDown = mkMMovePaneDown i
        , mMovePaneUp = mkMMovePaneUp i
        }
  traverse convertPane $ enumerate panes

makeClipboardsEdit ::
  MonadA m => Widget.R -> [Sugar.DefinitionN m ExprGuiM.Payload] -> ExprGuiM m (WidgetT m)
makeClipboardsEdit width clipboards = do
  clipboardsEdits <- traverse (makePaneWidget width) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Spacer.empty
    else ExprGuiM.widgetEnv $ BWidgets.makeTextViewWidget "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

makeSugarClipboards :: (MonadA m, Typeable1 m) => Anchors.CodeProps m -> CT m [Sugar.DefinitionN m ExprGuiM.Payload]
makeSugarClipboards cp =
  traverse (loadConvertDefI cp) =<<
  (lift . Transaction.getP . Anchors.clipboards) cp

make ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> Widget.Size -> Settings -> Guid ->
  StateT Cache (WidgetEnvT (T m)) (Widget (T m))
make cp size settings rootGuid = do
  (sugarPanes, sugarClipboards) <-
    mapStateT lift $
    (,) <$> makeSugarPanes cp rootGuid <*> makeSugarClipboards cp
  ExprGuiM.runWidget ExpressionEdit.make cp settings $ do
    panesEdit <- makePanesEdit width sugarPanes $ WidgetIds.fromGuid rootGuid
    clipboardsEdit <- makeClipboardsEdit width sugarClipboards
    return $
      Box.vboxAlign 0
      [ panesEdit
      , clipboardsEdit
      ]
  where
    width = size ^. Lens._1

makePanesEdit :: MonadA m => Widget.R -> [SugarPane m] -> Widget.Id -> ExprGuiM m (WidgetT m)
makePanesEdit width panes myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    paneEventMap pane = mconcat
      [ maybe mempty
        (Widget.keysEventMapMovesCursor (Config.closePaneKeys config)
         (E.Doc ["View", "Pane", "Close"]) . fmap WidgetIds.fromGuid) $ mDelPane pane
      , maybe mempty
        (Widget.keysEventMap (Config.movePaneDownKeys config)
         (E.Doc ["View", "Pane", "Move down"])) $ mMovePaneDown pane
      , maybe mempty
        (Widget.keysEventMap (Config.movePaneUpKeys config)
         (E.Doc ["View", "Pane", "Move up"])) $ mMovePaneUp pane
      ]
    makePaneEdit pane =
      (fmap . Widget.weakerEvents) (paneEventMap pane) .
      makePaneWidget width . spDef $ pane
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
      [ Widget.keysEventMapMovesCursor (Config.newDefinitionKeys config)
        (E.Doc ["Edit", "New definition"]) newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
         (E.Doc ["Navigation", "Go back"])) mJumpBack
      ]
  return $ Widget.weakerEvents panesEventMap panesWidget

makePaneWidget :: MonadA m => Widget.R -> Sugar.DefinitionN m ExprGuiM.Payload -> ExprGuiM m (Widget (T m))
makePaneWidget width rawDefS = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  infoMode <- (^. Settings.sInfoMode) <$> ExprGuiM.readSettings
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
    defS =
      case infoMode of
      Settings.Types -> rawDefS
      _ -> SugarRemoveTypes.nonHoleTypes <$> rawDefS
  fitToWidth width . colorize <$> DefinitionEdit.make defS

fitToWidth :: Widget.R -> Widget f -> Widget f
fitToWidth width w
  | ratio < 1 = w & Widget.scale (realToFrac ratio)
  | otherwise = w
  where
    ratio = width / w ^. Widget.wSize . Lens._1
