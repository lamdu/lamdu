{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, MultiParamTypeClasses #-}
module Lamdu.GUI.CodeEdit
    ( make
    , EvalResults
    , ReplEdit.ExportRepl(..), ExportActions(..)
    ) where

import           AST (_Pure)
import           AST.Term.Scheme (Scheme(..), QVars(..))
import           Algebra.Lattice (BoundedJoinSemiLattice(..))
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import qualified Data.Property as Property
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.GUI.CodeEdit.GotoDefinition as GotoDefinition
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.ReplEdit as ReplEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Collaboration as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name)
import           Lamdu.Settings (Settings)
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ExportActions m = ExportActions
    { exportAll :: IOTrans m ()
    , exportReplActions :: ReplEdit.ExportRepl m
    , exportDef :: V.Var -> IOTrans m ()
    , importAll :: FilePath -> IOTrans m ()
    }

type EvalResults m = CurAndPrev (EvalResults.EvalResults (ValI m))

make ::
    ( MonadTransaction m n, MonadReader env n, Has Config env
    , Has Cache.Functions env
    , Has Debug.Monitors env
    , Has Theme env, GuiState.HasState env
    , Has SugarConfig.Config env
    , Spacer.HasStdSpacing env
    , Has (EvalResults m) env
    , Has (ExportActions m) env
    , Has Settings env, HasStyle env
    , Has Hover.Style env, Has Menu.Config env
    , Has SearchMenu.TermStyle env
    , Has Annotations.Mode env
    , Element.HasAnimIdPrefix env
    , Language.HasLanguage env
    , HasCallStack
    ) =>
    Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) -> Widget.R ->
    n (StatusBar.StatusWidget (IOTrans m), Gui Widget (IOTrans m))
make cp gp width =
    do
        theExportActions <- Lens.view has
        env <- Lens.view id
        workArea <- loadWorkArea env cp & transaction
        gotoDefinition <-
            GotoDefinition.make (transaction (workArea ^. Sugar.waGlobals))
            <&> StatusBar.hoist IOTrans.liftTrans
        do
            replGui <-
                ReplEdit.make (exportReplActions theExportActions)
                (workArea ^. Sugar.waRepl)
            panesEdits <-
                workArea ^. Sugar.waPanes
                & traverse (makePaneEdit theExportActions)
            newDefinitionButton <-
                makeNewDefinitionButton cp <&> fmap IOTrans.liftTrans
                <&> Responsive.fromWidget
            eventMap <-
                panesEventMap theExportActions cp gp
                (workArea ^. Sugar.waRepl . Sugar.replVarInfo)
            Responsive.vboxSpaced
                ?? (replGui : panesEdits ++ [newDefinitionButton])
                <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> eventMap)
            & ExprGuiM.run ExpressionEdit.make BinderEdit.make gp env id
            & transaction
            <&> render
            <&> (^. Align.tValue)
            <&> (,) gotoDefinition
    where
        render gui =
            (gui ^. Responsive.rNarrow)
            Responsive.NarrowLayoutParams
            { _layoutWidth = width
            , _layoutNeedDisambiguation = False
            }

makePaneEdit ::
    Monad m =>
    ExportActions m ->
    Sugar.Pane (Name (T m)) (T m) (T m)
    (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload) ->
    ExprGuiM (T m) (T m) (Gui Responsive (IOTrans m))
makePaneEdit theExportActions pane =
    do
        env <- Lens.view id
        let titledCodeDoc titleLenses texts =
                E.toDoc env
                (titleLenses ++ map (has .) texts)
        let viewDoc = titledCodeDoc [has . MomentuTexts.view]
        let paneEventMap =
                [ pane ^. Sugar.paneClose & IOTrans.liftTrans
                  <&> WidgetIds.fromEntityId
                  & E.keysEventMapMovesCursor (paneConfig ^. Config.paneCloseKeys)
                    (viewDoc [Texts.pane, Texts.close])
                , pane ^. Sugar.paneMoveDown <&> IOTrans.liftTrans
                  & foldMap
                    (E.keysEventMap (paneConfig ^. Config.paneMoveDownKeys)
                    (viewDoc [Texts.pane, Texts.moveDown]))
                , pane ^. Sugar.paneMoveUp <&> IOTrans.liftTrans
                  & foldMap
                    (E.keysEventMap (paneConfig ^. Config.paneMoveUpKeys)
                    (viewDoc [Texts.pane, Texts.moveUp]))
                , exportDef theExportActions (pane ^. Sugar.paneDefinition . Sugar.drDefI)
                  & E.keysEventMap exportKeys
                    (E.toDoc (env ^. has)
                        [Texts.collaboration, Texts.exportDefToJSON])
                ] & mconcat
            defEventMap =
                do
                    Property.setP
                        (pane ^. Sugar.paneDefinition . Sugar.drDefinitionState & Property.MkProperty)
                        Sugar.DeletedDefinition
                    pane ^. Sugar.paneClose
                    <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor (Config.delKeys env)
                    (E.toDoc env
                        [ has . MomentuTexts.edit
                        , has . Texts.def
                        , has . MomentuTexts.delete
                        ])
            paneConfig = env ^. has . Config.pane
            exportKeys = env ^. has . Config.export . Config.exportKeys
        DefinitionEdit.make defEventMap (pane ^. Sugar.paneDefinition)
            <&> Lens.mapped %~ IOTrans.liftTrans
            <&> Widget.weakerEvents paneEventMap

makeNewDefinition ::
    Monad m => Anchors.CodeAnchors m -> ExprGuiM (T m) (T m) (T m Widget.Id)
makeNewDefinition cp =
    ExprGuiM.mkPrejumpPosSaver <&>
    \savePrecursor ->
    do
        savePrecursor
        holeI <- DataOps.newHole
        Definition
            (Definition.BodyExpr (Definition.Expr holeI mempty))
            ( _Pure # Scheme
                { _sForAlls =
                    T.Types (QVars (mempty & Lens.at "a" ?~ bottom)) (QVars mempty)
                , _sTyp = _Pure # T.TVar "a"
            }) ()
            & DataOps.newPublicDefinitionWithPane cp
    <&> WidgetIds.fromIRef

newDefinitionDoc ::
    ( MonadReader env m
    , Has (MomentuTexts.Texts Text) env, Has (Texts.Definitions Text) env
    ) => m E.Doc
newDefinitionDoc =
    Lens.view id
    <&> (`E.toDoc` [has . MomentuTexts.edit, has . Texts.newDefinition])

makeNewDefinitionButton ::
    Monad m =>
    Anchors.CodeAnchors m -> ExprGuiM (T m) (T m) (Gui Widget (T m))
makeNewDefinitionButton cp =
    do
        newDefId <- Element.subAnimId ?? ["New definition"] <&> Widget.Id
        newDefDoc <- newDefinitionDoc
        makeNewDefinition cp
            >>= Styled.actionable newDefId Texts.newDefinitionButton newDefDoc
            <&> (^. Align.tValue)

jumpBack :: Monad m => Anchors.GuiAnchors (T m) (T m) -> T m (Maybe (T m Widget.Id))
jumpBack gp =
    Property.getP (Anchors.preJumps gp)
    <&> \case
    [] -> Nothing
    (j:js) -> j <$ Property.setP (Anchors.preJumps gp) js & Just

panesEventMap ::
    Monad m =>
    ExportActions m -> Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) ->
    Sugar.VarInfo -> ExprGuiM (T m) (T m) (Gui EventMap (IOTrans m))
panesEventMap theExportActions cp gp replVarInfo =
    do
        env <- Lens.view id
        let exportConfig = env ^. has . Config.export
        mJumpBack <- jumpBack gp & transaction <&> fmap IOTrans.liftTrans
        newDefDoc <- newDefinitionDoc
        newDefinitionEventMap <-
            makeNewDefinition cp
            <&> E.keysEventMapMovesCursor
            (env ^. has . Config.pane . Config.newDefinitionKeys) newDefDoc
        let collaborationDoc = E.toDoc (env ^. has)
        mconcat
            [ newDefinitionEventMap <&> IOTrans.liftTrans
            , E.dropEventMap "Drag&drop JSON files"
                (collaborationDoc [Texts.collaboration, Texts.importJSON])
                (Just . traverse_ importAll)
                <&> fmap (\() -> mempty)
            , foldMap
              (E.keysEventMapMovesCursor (env ^. has . Config.previousCursorKeys)
               (E.toDoc env [has . MomentuTexts.navigation, has . Texts.goBack]))
                mJumpBack
            , E.keysEventMap (exportConfig ^. Config.exportAllKeys)
              (collaborationDoc [Texts.collaboration, Texts.exportEverythingToJSON])
                exportAll
            , importAll (exportConfig ^. Config.exportPath)
              & E.keysEventMap (exportConfig ^. Config.importKeys)
                (collaborationDoc [Texts.collaboration, Texts.importReplFromJSON])
            , case replVarInfo of
                Sugar.VarAction ->
                    E.keysEventMap (exportConfig ^. Config.executeKeys)
                    (E.toDoc (env ^. has) [Texts.execRepl])
                    (IOTrans.liftIO executeRepl)
                _ -> mempty
            ] & pure
    where
        executeRepl = exportReplActions theExportActions & ReplEdit.executeIOProcess
        ExportActions{importAll,exportAll} = theExportActions
