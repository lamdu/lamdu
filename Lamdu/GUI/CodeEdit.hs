{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable, RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.CodeEdit
    ( make
    , Env(..)
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (lift)
import           Control.MonadA (MonadA)
import           Data.List.Utils (insertAt, removeAt)
import qualified Data.Store.IRef as IRef
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (EvalResults(..))
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Load (loadDef)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.Names.Types (DefinitionN)
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

data Pane m = Pane
    { paneDefI :: DefI m
    , paneDel :: Maybe (T m Widget.Id)
    , paneMoveDown :: Maybe (T m ())
    , paneMoveUp :: Maybe (T m ())
    }

data Env m = Env
    { codeProps :: Anchors.CodeProps m
    , evalMap :: EvalResults (ExprIRef.ValI m)
    , config :: Config
    , settings :: Settings
    }

makePanes :: MonadA m => Transaction.Property m [DefI m] -> Widget.Id -> [Pane m]
makePanes (Property panes setPanes) rootId =
    panes ^@.. Lens.traversed <&> convertPane
    where
        mkMDelPane i
            | not (null panes) =
                Just $ do
                    let newPanes = removeAt i panes
                    setPanes newPanes
                    newPanes ^? Lens.ix i
                        & maybe rootId (WidgetIds.fromGuid . IRef.guid)
                        & return
            | otherwise = Nothing
        movePane oldIndex newIndex =
            insertAt newIndex item (before ++ after)
            & setPanes
            where
                (before, item:after) = splitAt oldIndex panes
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

processDefI ::
    MonadA m => Env m -> DefI m -> T m (DefinitionN m [Sugar.EntityId])
processDefI env defI =
    loadDef defI
    >>= SugarConvert.convertDefI (evalMap env) (codeProps env)
    >>= OrderTags.orderDef
    >>= PresentationModes.addToDef
    >>= AddNames.addToDef

processPane ::
    MonadA m => Env m -> Pane m ->
    T m (Pane m, DefinitionN m [Sugar.EntityId])
processPane env pane =
    processDefI env (paneDefI pane)
    <&> (,) pane

type Panes name m a = PanesP name m (Sugar.Expression name m a)
newtype PanesP name m expr = Panes
    { _panes :: [(Pane m, Sugar.Definition name m expr)]
    } deriving (Functor, Foldable, Traversable)

addNearestHoles ::
    MonadA m =>
    Panes name m [Sugar.EntityId] ->
    Panes name m ([Sugar.EntityId], NearestHoles.NearestHoles)
addNearestHoles (Panes panes) =
    panes
    <&> Lens._2 %~ NearestHoles.add traverse . (Lens.traversed %~ (<&> (,)))
    & Panes

make :: MonadA m => Env m -> Widget.Id -> WidgetEnvT (T m) (Widget (T m))
make env rootId =
    do
        prop <- lift $ Anchors.panes (codeProps env) ^. Transaction.mkProperty
        let sugarPanes = makePanes prop rootId
        Panes loadedPanes <-
            traverse (processPane env) sugarPanes
            & lift
            <&> addNearestHoles . Panes
        makePanesEdit env loadedPanes rootId

makeNewDefinitionEventMap ::
    MonadA m => Anchors.CodeProps m ->
    WidgetEnvT (T m) ([ModKey] -> Widget.EventHandlers (T m))
makeNewDefinitionEventMap cp =
    do
        curCursor <- WE.readCursor
        let newDefinition =
                do
                    newDefI <-
                        DataOps.newHole >>= DataOps.newPublicDefinitionWithPane "" cp
                    DataOps.savePreJumpPosition cp curCursor
                    return newDefI
                <&> DefinitionEdit.diveToNameEdit . WidgetIds.fromIRef
        return $ \newDefinitionKeys ->
            Widget.keysEventMapMovesCursor newDefinitionKeys
            (E.Doc ["Edit", "New definition"]) newDefinition

makePanesEdit ::
    MonadA m => Env m -> [(Pane m, ProcessedDef m)] ->
    Widget.Id -> WidgetEnvT (T m) (Widget (T m))
makePanesEdit env loadedPanes myId =
    do
        panesWidget <-
            case loadedPanes of
            [] ->
                makeNewDefinitionAction
                & WE.assignCursor myId newDefinitionActionId
            ((firstPane, _):_) ->
                do
                    newDefinitionAction <- makeNewDefinitionAction
                    loadedPanes
                        & traverse (makePaneEdit env Config.Pane{..})
                        <&> concatMap addSpacerAfter
                        <&> (++ [newDefinitionAction])
                        <&> Box.vboxAlign 0
                & (WE.assignCursor myId . WidgetIds.fromIRef . paneDefI) firstPane
        eventMap <- panesEventMap env
        panesWidget
            & Widget.weakerEvents eventMap
            & return
    where
        newDefinitionActionId = Widget.joinId myId ["NewDefinition"]
        makeNewDefinitionAction =
            do
                newDefinitionEventMap <- makeNewDefinitionEventMap (codeProps env)
                BWidgets.makeFocusableTextView "New..." newDefinitionActionId
                    & WE.localEnv (WE.setTextColor newDefinitionActionColor)
                    <&> Widget.weakerEvents
                        (newDefinitionEventMap newDefinitionButtonPressKeys)
        addSpacerAfter x = [x, Spacer.makeWidget 50]
        Config.Pane{..} = Config.pane $ config env

makePaneEdit ::
    MonadA m =>
    Env m -> Config.Pane -> (Pane m, ProcessedDef m) ->
    WidgetEnvT (T m) (Widget (T m))
makePaneEdit env Config.Pane{..} (pane, defS) =
    makePaneWidget env defS
    <&> Widget.weakerEvents paneEventMap
    where
        paneEventMap =
            [ maybe mempty
                (Widget.keysEventMapMovesCursor paneCloseKeys
                  (E.Doc ["View", "Pane", "Close"])) $ paneDel pane
            , maybe mempty
                (Widget.keysEventMap paneMoveDownKeys
                  (E.Doc ["View", "Pane", "Move down"])) $ paneMoveDown pane
            , maybe mempty
                (Widget.keysEventMap paneMoveUpKeys
                  (E.Doc ["View", "Pane", "Move up"])) $ paneMoveUp pane
            ] & mconcat

panesEventMap ::
    MonadA m => Env m -> WidgetEnvT (T m) (Widget.EventHandlers (T m))
panesEventMap Env{..} =
    do
        mJumpBack <- lift $ DataOps.jumpBack codeProps
        newDefinitionEventMap <- makeNewDefinitionEventMap codeProps
        return $ mconcat
            [ newDefinitionEventMap newDefinitionKeys
            , maybe mempty
                (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
                  (E.Doc ["Navigation", "Go back"])) mJumpBack
            ]
    where
        Config.Pane{..} = Config.pane config

makePaneWidget ::
    MonadA m => Env m -> ProcessedDef m -> WidgetEnvT (T m) (Widget (T m))
makePaneWidget env defS =
    DefinitionEdit.make (codeProps env) (config env) (settings env) defS
    <&> colorize
    where
        Config.Pane{..} = Config.pane (config env)
        colorize widget
            | widget ^. Widget.isFocused = colorizeActivePane widget
            | otherwise = colorizeInactivePane widget
        colorizeActivePane =
            Widget.backgroundColor
            (Config.layerActivePane (Config.layers (config env)))
            WidgetIds.activePaneBackground paneActiveBGColor
        colorizeInactivePane = Widget.tint paneInactiveTintColor
