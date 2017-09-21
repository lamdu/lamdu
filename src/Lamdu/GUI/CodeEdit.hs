{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields, MultiParamTypeClasses #-}
module Lamdu.GUI.CodeEdit
    ( make
    , HasEvalResults(..)
    , ReplEdit.ExportRepl(..), ExportActions(..), HasExportActions(..)
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Config (config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI, ValI)
import           Lamdu.GUI.CodeEdit.Settings (HasSettings)
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.IOTrans (IOTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.RedundantAnnotations as RedundantAnnotations
import qualified Lamdu.GUI.ReplEdit as ReplEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.Names.Types (Name)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data ExportActions m = ExportActions
    { exportAll :: IOTrans m ()
    , exportReplActions :: ReplEdit.ExportRepl m
    , exportDef :: DefI m -> IOTrans m ()
    , importAll :: FilePath -> IOTrans m ()
    }

class HasEvalResults env m where
    evalResults :: Lens' env (CurAndPrev (EvalResults (ValI m)))

class HasExportActions env m where exportActions :: Lens' env (ExportActions m)


toExprGuiMPayload :: ([Sugar.EntityId], NearestHoles) -> ExprGuiT.Payload
toExprGuiMPayload (entityIds, nearestHoles) =
    ExprGuiT.emptyPayload nearestHoles & ExprGuiT.plStoredEntityIds .~ entityIds

traverseAddNearestHoles ::
    Traversable t =>
    t (Sugar.Expression name m a) ->
    t (Sugar.Expression name m (a, NearestHoles))
traverseAddNearestHoles binder =
    binder
    <&> Lens.mapped %~ (,)
    & NearestHoles.add traverse

exprAddNearestHoles ::
    Sugar.Expression name m a ->
    Sugar.Expression name m (a, NearestHoles)
exprAddNearestHoles expr =
    Identity expr
    & traverseAddNearestHoles
    & runIdentity

postProcessExpr ::
    Sugar.Expression name m ([Sugar.EntityId], NearestHoles) ->
    Sugar.Expression name m ExprGuiT.Payload
postProcessExpr expr =
    expr
    <&> toExprGuiMPayload
    & RedundantAnnotations.markAnnotationsToDisplay

loadWorkArea ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m ->
    ExprGuiM m (Sugar.WorkArea (Name m) m ExprGuiT.Payload)
loadWorkArea theEvalResults theCodeAnchors =
    do
        Sugar.WorkArea { _waPanes, _waRepl } <-
            SugarConvert.loadWorkArea theEvalResults theCodeAnchors
            >>= AddNames.addToWorkArea
            & transaction
        Sugar.WorkArea
            (_waPanes <&> Sugar.paneDefinition %~ fmap postProcessExpr . traverseAddNearestHoles)
            (_waRepl & exprAddNearestHoles & postProcessExpr)
            & pure

make ::
    ( MonadTransaction m n, MonadReader env n, Config.HasConfig env
    , Theme.HasTheme env, Widget.HasCursor env, TextEdit.HasStyle env
    , Spacer.HasStdSpacing env, HasEvalResults env m, HasExportActions env m
    , HasSettings env, HasStyle env
    ) =>
    Anchors.CodeAnchors m -> Widget.R ->
    n (Widget (IOTrans m Widget.EventResult))
make theCodeAnchors width =
    do
        theEvalResults <- Lens.view evalResults
        theExportActions <- Lens.view exportActions
        do
            workArea <- loadWorkArea theEvalResults theCodeAnchors
            replGui <-
                ReplEdit.make (exportReplActions theExportActions)
                (workArea ^. Sugar.waRepl)
            panesEdits <-
                workArea ^. Sugar.waPanes
                & traverse (makePaneEdit theExportActions)
            newDefinitionButton <- makeNewDefinitionButton <&> fmap IOTrans.liftTrans <&> Responsive.fromWidget
            eventMap <- panesEventMap theExportActions theCodeAnchors
            Responsive.vboxSpaced
                ?? (replGui : panesEdits ++ [newDefinitionButton])
                <&> E.weakerEvents eventMap
            & ExprGuiM.run ExpressionEdit.make theCodeAnchors
            <&> ExpressionGui.render width
            <&> (^. Align.tValue)

makePaneEdit ::
    Monad m =>
    ExportActions m ->
    Sugar.Pane (Name m) m ExprGuiT.Payload ->
    ExprGuiM m (Responsive (IOTrans m Widget.EventResult))
makePaneEdit theExportActions pane =
    do
        theConfig <- Lens.view config
        let paneEventMap =
                [ pane ^. Sugar.paneClose & IOTrans.liftTrans
                  <&> WidgetIds.fromEntityId
                  & Widget.keysEventMapMovesCursor paneCloseKeys
                    (E.Doc ["View", "Pane", "Close"])
                , do
                      Transaction.setP (pane ^. Sugar.paneDefinition . Sugar.drDefinitionState)
                          Sugar.DeletedDefinition
                      pane ^. Sugar.paneClose
                  & IOTrans.liftTrans
                  <&> WidgetIds.fromEntityId
                  & Widget.keysEventMapMovesCursor (Config.delKeys theConfig)
                    (E.Doc ["Edit", "Definition", "Delete"])
                , pane ^. Sugar.paneMoveDown <&> IOTrans.liftTrans
                  & maybe mempty
                    (Widget.keysEventMap paneMoveDownKeys
                    (E.Doc ["View", "Pane", "Move down"]))
                , pane ^. Sugar.paneMoveUp <&> IOTrans.liftTrans
                  & maybe mempty
                    (Widget.keysEventMap paneMoveUpKeys
                    (E.Doc ["View", "Pane", "Move up"]))
                , exportDef theExportActions (pane ^. Sugar.paneDefinition . Sugar.drDefI)
                  & Widget.keysEventMap exportKeys
                    (E.Doc ["Collaboration", "Export definition to JSON file"])
                ] & mconcat
            Config.Pane{paneCloseKeys, paneMoveDownKeys, paneMoveUpKeys} = Config.pane theConfig
            exportKeys = Config.exportKeys (Config.export theConfig)
        DefinitionEdit.make (pane ^. Sugar.paneDefinition)
            <&> Lens.mapped %~ IOTrans.liftTrans
            <&> E.weakerEvents paneEventMap

makeNewDefinitionEventMap ::
    (Monad m, MonadReader env n, Widget.HasCursor env) =>
    Anchors.CodeAnchors m ->
    n ([MetaKey] -> Widget.EventMap (T m Widget.EventResult))
makeNewDefinitionEventMap cp =
    do
        curCursor <- Lens.view Widget.cursor
        let newDefinition =
                do
                    holeI <- DataOps.newHole
                    newDefI <-
                        Definition
                        (Definition.BodyExpr (Definition.Expr holeI mempty))
                        Scheme.any ()
                        & DataOps.newPublicDefinitionWithPane "" cp
                    DataOps.savePreJumpPosition cp curCursor
                    return newDefI
                <&> WidgetIds.nameEditOf . WidgetIds.fromIRef
        return $ \newDefinitionKeys ->
            Widget.keysEventMapMovesCursor newDefinitionKeys
            (E.Doc ["Edit", "New definition"]) newDefinition

makeNewDefinitionButton :: Monad m => ExprGuiM m (Widget (T m Widget.EventResult))
makeNewDefinitionButton =
    do
        anchors <- ExprGuiM.readCodeAnchors
        newDefinitionEventMap <- makeNewDefinitionEventMap anchors

        Config.Pane{newDefinitionButtonPressKeys} <- Lens.view Config.config <&> Config.pane
        color <- Lens.view Theme.theme <&> Theme.newDefinitionActionColor

        TextView.makeFocusableLabel "New..."
            & Reader.local (TextView.color .~ color)
            <&> (^. Align.tValue)
            <&> E.weakerEvents (newDefinitionEventMap newDefinitionButtonPressKeys)

panesEventMap ::
    Monad m =>
    ExportActions m -> Anchors.CodeAnchors m ->
    ExprGuiM m (Widget.EventMap (IOTrans m Widget.EventResult))
panesEventMap theExportActions theCodeAnchors =
    do
        theConfig <- Lens.view config
        let Config.Export{exportPath,importKeys,exportAllKeys} = Config.export theConfig
        mJumpBack <-
            DataOps.jumpBack theCodeAnchors & transaction <&> fmap IOTrans.liftTrans
        newDefinitionEventMap <- makeNewDefinitionEventMap theCodeAnchors
        return $ mconcat
            [ newDefinitionEventMap (Config.newDefinitionKeys (Config.pane theConfig))
              <&> IOTrans.liftTrans
            , E.dropEventMap "Drag&drop JSON files"
              (E.Doc ["Collaboration", "Import JSON file"]) (Just . traverse_ importAll)
              <&> fmap (\() -> mempty)
            , maybe mempty
              (Widget.keysEventMapMovesCursor (Config.previousCursorKeys theConfig)
               (E.Doc ["Navigation", "Go back"])) mJumpBack
            , Widget.keysEventMap exportAllKeys
              (E.Doc ["Collaboration", "Export everything to JSON file"]) exportAll
            , importAll exportPath
              & Widget.keysEventMap importKeys
                (E.Doc ["Collaboration", "Import repl from JSON file"])
            ]
    where
        ExportActions{importAll,exportAll} = theExportActions
