{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit
    ( make
    , Env(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (lift)
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import           Data.List (intersperse)
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
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI, ValI)
import           Lamdu.Expr.Load (loadDef, loadExprProperty)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.RedundantAnnotations as RedundantAnnotations
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.Names.Types (DefinitionN)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

data Pane m = Pane
    { paneDefI :: DefI m
    , paneDel :: Maybe (T m Widget.Id)
    , paneMoveDown :: Maybe (T m ())
    , paneMoveUp :: Maybe (T m ())
    }

data Env m = Env
    { codeProps :: Anchors.CodeProps m
    , evalResults :: CurAndPrev (EvalResults (ValI m))
    , config :: Config
    , settings :: Settings
    }

makePanes :: MonadA m => Widget.Id -> Transaction.Property m [DefI m] -> [Pane m]
makePanes defaultDelDest (Property panes setPanes) =
    panes ^@.. Lens.traversed <&> convertPane
    where
        mkMDelPane i
            | not (null panes) =
                Just $ do
                    let newPanes = removeAt i panes
                    setPanes newPanes
                    newPanes ^? Lens.ix i
                        & maybe defaultDelDest (WidgetIds.fromGuid . IRef.guid)
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

toExprGuiMPayload :: ([Sugar.EntityId], NearestHoles) -> ExprGuiT.Payload
toExprGuiMPayload (entityIds, nearestHoles) =
    ExprGuiT.emptyPayload nearestHoles & ExprGuiT.plStoredEntityIds .~ entityIds

processPane ::
    MonadA m => Env m -> Pane m ->
    T m (Pane m, DefinitionN m ExprGuiT.Payload)
processPane env pane =
    paneDefI pane
    & loadDef
    >>= SugarConvert.convertDefI (evalResults env) (codeProps env)
    >>= OrderTags.orderDef
    >>= PresentationModes.addToDef
    >>= AddNames.addToDef
    <&> NearestHoles.add traverse . (Lens.traversed %~ (<&> (,)))
    <&> Lens.mapped . Lens.mapped %~ toExprGuiMPayload
    <&> Lens.mapped %~ RedundantAnnotations.markAnnotationsToDisplay
    <&> (,) pane

processExpr ::
    MonadA m => Env m -> Transaction.Property m (ValI m) ->
    T m (ExprGuiT.SugarExpr m)
processExpr env expr =
    loadExprProperty expr
    >>= SugarConvert.convertExpr (evalResults env) (codeProps env)
    >>= OrderTags.orderExpr
    >>= PresentationModes.addToExpr
    >>= AddNames.addToExpr
    <&> Lens.mapped %~ (,)
    <&> runIdentity . NearestHoles.add traverse . Identity
    <&> Lens.mapped %~ toExprGuiMPayload
    <&> RedundantAnnotations.markAnnotationsToDisplay

make :: MonadA m => Env m -> Widget.Id -> WidgetEnvT (T m) (Widget (T m))
make env rootId =
    do
        replExpr <-
            getProp Anchors.repl >>= lift . processExpr env
        panes <- getProp Anchors.panes <&> makePanes rootId
        gui env rootId replExpr panes
    where
        getProp f = f (codeProps env) ^. Transaction.mkProperty & lift

makePaneEdit ::
    MonadA m =>
    Env m -> Config.Pane -> (Pane m, DefinitionN m ExprGuiT.Payload) ->
    ExprGuiM m (Widget (T m))
makePaneEdit env Config.Pane{..} (pane, defS) =
    makePaneWidget (config env) defS
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

makeNewDefinitionButton :: MonadA m => Env m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNewDefinitionButton env myId =
    do
        newDefinitionEventMap <- makeNewDefinitionEventMap (codeProps env)
        BWidgets.makeFocusableTextView "New..." newDefinitionButtonId
            & WE.localEnv (WE.setTextColor newDefinitionActionColor)
            <&> Widget.weakerEvents
                (newDefinitionEventMap newDefinitionButtonPressKeys)
    & ExprGuiM.widgetEnv
    where
        Config.Pane{..} = Config.pane $ config env
        newDefinitionButtonId = Widget.joinId myId ["NewDefinition"]

makeReplEdit ::
    MonadA m => Widget.Id -> ExprGuiT.SugarExpr m -> ExprGuiM m (Widget (T m))
makeReplEdit myId replExpr =
    [ ExpressionGui.makeLabel "⋙" (Widget.toAnimId myId)
    , ExprGuiM.makeSubexpression id replExpr
    ] & sequenceA
    >>= ExpressionGui.hboxSpaced
    <&> (^. ExpressionGui.egWidget)

gui ::
    MonadA m =>
    Env m -> Widget.Id -> ExprGuiT.SugarExpr m -> [Pane m] ->
    WidgetEnvT (T m) (Widget (T m))
gui env rootId replExpr panes =
    do
        replEdit <- makeReplEdit rootId replExpr
        panesEdits <-
            panes
            & ExprGuiM.transaction . traverse (processPane env)
            >>= traverse (makePaneEdit env (Config.pane (config env)))
        newDefinitionButton <- makeNewDefinitionButton env rootId
        eventMap <- panesEventMap env & ExprGuiM.widgetEnv
        [replEdit] ++ panesEdits ++ [newDefinitionButton]
            & intersperse space
            & Box.vboxAlign 0
            & Widget.weakerEvents eventMap
            & return
    & ExprGuiM.assignCursor rootId replId
    & ExprGuiM.run ExpressionEdit.make (codeProps env) (config env) (settings env)
    where
        space = Spacer.makeWidget 50
        replId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

panesEventMap ::
    MonadA m => Env m -> WidgetEnvT (T m) (Widget.EventHandlers (T m))
panesEventMap Env{..} =
    do
        mJumpBack <- lift $ DataOps.jumpBack codeProps
        newDefinitionEventMap <- makeNewDefinitionEventMap codeProps
        return $ mconcat
            [ newDefinitionEventMap (Config.newDefinitionKeys (Config.pane config))
            , maybe mempty
                (Widget.keysEventMapMovesCursor (Config.previousCursorKeys config)
                  (E.Doc ["Navigation", "Go back"])) mJumpBack
            ]

makePaneWidget ::
    MonadA m => Config -> DefinitionN m ExprGuiT.Payload -> ExprGuiM m (Widget (T m))
makePaneWidget conf defS =
    DefinitionEdit.make defS <&> colorize
    where
        Config.Pane{..} = Config.pane conf
        colorize widget
            | widget ^. Widget.isFocused = colorizeActivePane widget
            | otherwise = colorizeInactivePane widget
        colorizeActivePane =
            Widget.backgroundColor
            (Config.layerActivePane (Config.layers conf))
            WidgetIds.activePaneBackground paneActiveBGColor
        colorizeInactivePane = Widget.tint paneInactiveTintColor
