{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit (make, Env(..)) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Foldable (Foldable)
import Data.List (intersperse)
import Data.List.Utils (insertAt, removeAt)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable, traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Load (loadDef)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import Lamdu.Sugar.AddNames.Types (DefinitionN)
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
import qualified Lamdu.Sugar.AddNames as AddNames
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.Types as Sugar

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
  panes ^@.. Lens.traversed <&> convertPane
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

type ProcessedDef m = DefinitionN m ([Sugar.EntityId], NearestHoles.NearestHoles)

makeClipboardsEdit ::
  MonadA m => Env m ->
  [ProcessedDef m] ->
  WidgetEnvT (T m) (Widget (T m))
makeClipboardsEdit env clipboards = do
  clipboardsEdits <- traverse (makePaneWidget env) clipboards
  clipboardTitle <-
    if null clipboardsEdits
    then return Widget.empty
    else BWidgets.makeTextViewWidget "Clipboards:" ["clipboards title"]
  return . Box.vboxAlign 0 $ clipboardTitle : clipboardsEdits

getClipboards :: MonadA m => Anchors.CodeProps m -> T m [DefI m]
getClipboards = Transaction.getP . Anchors.clipboards

processDefI ::
  MonadA m => Env m -> DefI m -> T m (DefinitionN m [Sugar.EntityId])
processDefI env defI =
  loadDef defI
  >>= SugarConvert.convertDefI (codeProps env)
  >>= OrderTags.orderDef
  >>= AddNames.addToDef

processPane ::
  MonadA m => Env m -> Pane m ->
  T m (Pane m, DefinitionN m [Sugar.EntityId])
processPane env pane =
  processDefI env (paneDefI pane)
  <&> (,) pane

type PanesAndClipboards name m a =
    PanesAndClipboardsP name m (Sugar.Expression name m a)
data PanesAndClipboardsP name m expr =
  PanesAndClipboards
  { _panes :: [(Pane m, Sugar.Definition name m expr)]
  , _clipboards :: [Sugar.Definition name m expr]
  } deriving (Functor, Foldable, Traversable)

addNearestHoles ::
  MonadA m =>
  PanesAndClipboards name m [Sugar.EntityId] ->
  PanesAndClipboards name m ([Sugar.EntityId], NearestHoles.NearestHoles)
addNearestHoles pcs =
  pcs
  <&> Lens.mapped . Sugar.plData %~ (,)
  & NearestHoles.add traverse

make :: MonadA m => Env m -> Guid -> WidgetEnvT (T m) (Widget (T m))
make env rootGuid = do
  prop <- lift $ Anchors.panes (codeProps env) ^. Transaction.mkProperty

  let sugarPanes = makePanes prop rootGuid
  sugarClipboards <- lift $ getClipboards $ codeProps env

  PanesAndClipboards loadedPanes loadedClipboards <-
    PanesAndClipboards
    <$> traverse (processPane env) sugarPanes
    <*> traverse (processDefI env) sugarClipboards
    & lift
    <&> addNearestHoles

  panesEdit <- makePanesEdit env loadedPanes $ WidgetIds.fromGuid rootGuid
  clipboardsEdit <- makeClipboardsEdit env loadedClipboards

  return $
    Box.vboxAlign 0
    [ panesEdit
    , clipboardsEdit
    ]

makeNewDefinition ::
  MonadA m => Anchors.CodeProps m ->
  WidgetEnvT (T m) (T m Widget.Id)
makeNewDefinition cp = do
  curCursor <- WE.readCursor
  return $ do
    newDefI <- DataOps.newPublicDefinition cp ""
    DataOps.newPane cp newDefI
    DataOps.savePreJumpPosition cp curCursor
    return . DefinitionEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI

makePanesEdit ::
  MonadA m => Env m -> [(Pane m, ProcessedDef m)] ->
  Widget.Id -> WidgetEnvT (T m) (Widget (T m))
makePanesEdit env loadedPanes myId = do
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
    makePaneEdit (pane, defS) =
      makePaneWidget env defS
      <&> Widget.weakerEvents (paneEventMap pane)
  panesWidget <-
    case loadedPanes of
    [] -> BWidgets.makeFocusableTextView "<No panes>" myId
    ((firstPane, _):_) ->
      do
        definitionEdits <- traverse makePaneEdit loadedPanes
        return . Box.vboxAlign 0 $ intersperse (Spacer.makeWidget 50) definitionEdits
      & (WE.assignCursor myId . WidgetIds.fromIRef . paneDefI) firstPane
  eventMap <- panesEventMap env
  panesWidget
    & Widget.weakerEvents eventMap
    & return

panesEventMap ::
  MonadA m => Env m -> WidgetEnvT (T m) (Widget.EventHandlers (T m))
panesEventMap env =
  do
    config <- WE.readConfig
    let Config.Pane{..} = Config.pane config
    mJumpBack <- lift . DataOps.jumpBack $ codeProps env
    newDefinition <- makeNewDefinition $ codeProps env
    return $ mconcat
      [ Widget.keysEventMapMovesCursor newDefinitionKeys
        (E.Doc ["Edit", "New definition"]) newDefinition
      , maybe mempty
        (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
         (E.Doc ["Navigation", "Go back"])) mJumpBack
      ]

makePaneWidget ::
  MonadA m => Env m -> ProcessedDef m -> WidgetEnvT (T m) (Widget (T m))
makePaneWidget env defS = do
  config <- WE.readConfig
  let
    Config.Pane{..} = Config.pane config
    colorize widget
      | widget ^. Widget.wIsFocused = colorizeActivePane widget
      | otherwise = colorizeInactivePane widget
    colorizeActivePane =
      Widget.backgroundColor
      (Config.layerActivePane (Config.layers config))
      WidgetIds.activePaneBackground paneActiveBGColor
    colorizeInactivePane =
      Widget.wFrame . Anim.unitImages %~ Draw.tint paneInactiveTintColor
  DefinitionEdit.make (codeProps env) (settings env) defS
    <&> fitToWidth (totalWidth env) . colorize

fitToWidth :: Widget.R -> Widget f -> Widget f
fitToWidth width w
  | ratio < 1 = w & Widget.scale (realToFrac ratio)
  | otherwise = w
  where
    ratio = width / w ^. Widget.wSize . Lens._1
