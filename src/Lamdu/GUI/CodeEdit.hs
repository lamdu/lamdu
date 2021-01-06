{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.CodeEdit
    ( make
    , Model
    , EvalResults
    , ReplEdit.ExportRepl(..), ExportActions(..)

    , -- exported for tests
      makePaneBodyEdit
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Control.Monad.Once (OnceT, evalOnceT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Property as Property
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Hyper.Type.AST.Scheme (Scheme(..), QVars(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (Tag, IsOperator, TextsInLang, getTagName)
import qualified Lamdu.Eval.Results as EvalResults
import qualified Lamdu.GUI.CodeEdit.GotoDefinition as GotoDefinition
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.Expr as ExpressionEdit
import qualified Lamdu.GUI.Expr.BinderEdit as BinderEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.ReplEdit as ReplEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TagPane as TagPaneEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Collaboration as Texts
import qualified Lamdu.I18N.Definitions as Texts
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import           Lamdu.Settings (Settings)
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ExportActions m = ExportActions
    { exportAll :: IOTrans m ()
    , exportReplActions :: ReplEdit.ExportRepl m
    , exportDef :: V.Var -> IOTrans m ()
    , exportTag :: T.Tag -> IOTrans m ()
    , importAll :: FilePath -> IOTrans m ()
    }

type EvalResults = CurAndPrev EvalResults.EvalResults

type Model env m =
    OnceT (T m)
    ( (Tag -> (IsOperator, TextsInLang)) -> env ->
        OnceT (T m)
        ( Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) Name (OnceT (T m)) (T m)
            (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) Name (OnceT (T m)) (T m)
            , (Sugar.ParenInfo, [Sugar.EntityId])
            )
        )
    )

make ::
    ( Has Config env
    , Has Theme env, GuiState.HasState env
    , Spacer.HasStdSpacing env
    , Has (ExportActions m) env
    , Has Settings env, HasStyle env
    , Has Hover.Style env, Has Menu.Config env
    , Has SearchMenu.TermStyle env
    , Element.HasAnimIdPrefix env
    , Language.HasLanguage env
    , Monad m
    ) =>
    Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) -> Widget.R -> Model env m ->
    ReaderT env (OnceT (T m)) (StatusBar.StatusWidget (IOTrans m), Widget (IOTrans m))
make cp gp width mkWorkArea =
    do
        theExportActions <- Lens.view has
        env <- Lens.view id
        workArea <-
            mkWorkArea >>= (\x -> x (getTagName env) env)
            <&> Lens.mapped . Lens.mapped %~ uncurry ExprGui.GuiPayload
            & lift
        gotoDefinition <-
            GotoDefinition.make (workArea ^. Sugar.waGlobals & lift)
            <&> StatusBar.hoist IOTrans.liftTrans
        assocTagName <- DataOps.assocTagName
        do
            replGui <-
                ReplEdit.make (exportReplActions theExportActions)
                (workArea ^. Sugar.waRepl)
            panesEdits <-
                workArea ^. Sugar.waPanes
                & traverse (makePaneEdit theExportActions)
            newDefinitionButton <-
                makeNewDefinitionButton cp
                <&> Widget.updates %~ IOTrans.liftTrans
                <&> Responsive.fromWidget
            eventMap <-
                panesEventMap theExportActions cp gp
                (workArea ^. Sugar.waRepl . Sugar.replVarInfo)
            Responsive.vboxSpaced
                ?? (replGui : panesEdits ++ [newDefinitionButton])
                <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> eventMap)
            & GuiM.run assocTagName ExpressionEdit.make BinderEdit.make
                (Anchors.onGui (Property.mkProperty %~ lift) gp) env evalOnceT
            & lift
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

exportPaneEventMap ::
    ( Functor m
    , Has Config env
    , Has (Texts.Collaboration Text) env
    ) =>
    env -> ExportActions m -> Sugar.PaneBody v name i o dummy ->
    EventMap (IOTrans m GuiState.Update)
exportPaneEventMap env theExportActions paneBody =
    case paneBody of
    Sugar.PaneDefinition def ->
        exportEventMap exportDef (def ^. Sugar.drDefI) Texts.exportDefToJSON
    Sugar.PaneTag tag ->
        exportEventMap exportTag (tag ^. Sugar.tpTag . Sugar.tagVal) Texts.exportTagToJSON
    where
        exportKeys = env ^. has . Config.export . Config.exportKeys
        exportEventMap act arg docLens =
            act theExportActions arg
            & E.keysEventMap exportKeys
            (E.toDoc (env ^. has) [Texts.collaboration, docLens])

makePaneBodyEdit ::
    ( Monad i, Monad o
    , Grid.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env, Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env, Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env, Has (Texts.Navigation Text) env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    ExprGui.Top Sugar.Pane i o -> GuiM env i o (Responsive o)
makePaneBodyEdit pane =
    case pane ^. Sugar.paneBody of
    Sugar.PaneTag tag -> TagPaneEdit.make tag <&> Responsive.fromWidget
    Sugar.PaneDefinition def ->
        do
            env <- Lens.view id
            let eventMap =
                    do
                        (def ^. Sugar.drDefinitionState . Property.pSet) Sugar.DeletedDefinition
                        pane ^. Sugar.paneClose
                    <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor (Config.delKeys env)
                    (E.toDoc env
                        [ has . MomentuTexts.edit
                        , has . Texts.def
                        , has . MomentuTexts.delete
                        ])
            DefinitionEdit.make eventMap def

makePaneEdit ::
    (Monad m, Language.HasLanguage env) =>
    ExportActions m ->
    ExprGui.Top Sugar.Pane (OnceT (T m)) (T m) ->
    GuiM env (OnceT (T m)) (T m) (Responsive (IOTrans m))
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
                  & E.keysEventMapMovesCursor
                    (paneConfig ^. Config.paneCloseKeys <> Config.delKeys env)
                    (viewDoc [Texts.pane, Texts.close])
                , pane ^. Sugar.paneMoveDown <&> IOTrans.liftTrans
                  & foldMap
                    (E.keysEventMap (paneConfig ^. Config.paneMoveDownKeys)
                    (viewDoc [Texts.pane, Texts.moveDown]))
                , pane ^. Sugar.paneMoveUp <&> IOTrans.liftTrans
                  & foldMap
                    (E.keysEventMap (paneConfig ^. Config.paneMoveUpKeys)
                    (viewDoc [Texts.pane, Texts.moveUp]))
                , exportPaneEventMap env theExportActions (pane ^. Sugar.paneBody)
                ] & mconcat
            paneConfig = env ^. has . Config.pane
        makePaneBodyEdit pane
            <&> Widget.updates %~ IOTrans.liftTrans
            <&> Widget.weakerEvents paneEventMap

makeNewDefinition ::
    Monad m => Anchors.CodeAnchors m -> GuiM env (OnceT (T m)) (T m) (T m Widget.Id)
makeNewDefinition cp =
    GuiM.mkPrejumpPosSaver <&>
    \savePrecursor ->
    do
        savePrecursor
        holeI <- DataOps.newHole
        Definition
            (Definition.BodyExpr (Definition.Expr holeI mempty))
            ( _Pure # Scheme
                { _sForAlls =
                    T.Types (QVars ("a" ~~> mempty)) (QVars mempty)
                , _sTyp = _Pure # T.TVar "a"
            }) ()
            & DataOps.newPublicDefinitionWithPane cp
    <&> WidgetIds.fromIRef

newDefinitionDoc ::
    ( MonadReader env m
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    ) => m E.Doc
newDefinitionDoc =
    Lens.view id
    <&> (`E.toDoc` [has . MomentuTexts.edit, has . Texts.new])

makeNewDefinitionButton ::
    (Monad m, Language.HasLanguage env) =>
    Anchors.CodeAnchors m -> GuiM env (OnceT (T m)) (T m) (Widget (T m))
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
    (Monad m, Language.HasLanguage env) =>
    ExportActions m -> Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) ->
    Sugar.VarInfo -> GuiM env (OnceT (T m)) (T m) (EventMap (IOTrans m GuiState.Update))
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
                Sugar.VarNominal (Sugar.TId _ tid) | tid == Builtins.mutTid ->
                    E.keysEventMap (exportConfig ^. Config.executeKeys)
                    (E.toDoc (env ^. has) [Texts.execRepl])
                    (IOTrans.liftIO executeRepl)
                _ -> mempty
            ] & pure
    where
        executeRepl = exportReplActions theExportActions & ReplEdit.executeIOProcess
        ExportActions{importAll,exportAll} = theExportActions
