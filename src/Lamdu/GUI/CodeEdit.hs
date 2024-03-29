{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}

module Lamdu.GUI.CodeEdit
    ( make
    , Model
    , EvalResults
    , ExportActions(..)

    , -- exported for tests
      makePaneBodyEdit
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.FastWriter (runWriterT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Property (Property)
import qualified Data.Property as Property
import           Data.Tuple (swap)
import           GUI.Momentu (Widget, EventMap, Responsive, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (Tag, IsOperator, TextsInLang, getTagName)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.GUI.CodeEdit.GotoDefinition as GotoDefinition
import           Lamdu.GUI.Definition (DefRes, _DefRes)
import qualified Lamdu.GUI.Definition as DefinitionEdit
import qualified Lamdu.GUI.Expr as ExpressionEdit
import qualified Lamdu.GUI.Expr.BinderEdit as BinderEdit
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.NominalPane as NominalPane
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TagPane as TagPaneEdit
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Collaboration as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Internal.EntityId (ofTaggedEntity)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ExportActions m = ExportActions
    { exportAll :: IOTrans m Update
    , exportDef :: V.Var -> IOTrans m Update
    , exportDefToJS :: V.Var -> IOTrans m Update
    , executeDef :: V.Var -> IO ()
    , exportTag :: T.Tag -> IOTrans m Update
    , exportNominal :: T.NominalId -> IOTrans m Update
    , importAll :: FilePath -> IOTrans m ()
    }

type EvalResults = CurAndPrev EvalResults.EvalResults

type Model env m =
    OnceT (T m)
    ( (Tag -> (IsOperator, TextsInLang)) -> env ->
        OnceT (T m)
        ( Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) Name (OnceT (T m)) (T m)
            (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T m))) Name) (T m))
        )
    )

make ::
    _ =>
    Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) -> Widget.R -> Model env m ->
    ReaderT env (OnceT (T m)) (StatusBar.StatusWidget (IOTrans m), Widget (IOTrans m))
make cp gp width mkWorkArea =
    do
        theExportActions <- Lens.view has
        env <- Lens.view id
        workArea <- mkWorkArea >>= (\x -> x (getTagName env) env) & lift
        gotoDefinition <-
            GotoDefinition.make (workArea ^. Sugar.waOpenPane) (workArea ^. Sugar.waGlobals)
            <&> StatusBar.hoist IOTrans.liftTrans
        assocTagName <- DataOps.assocTagName
        do
            newDefId <- Element.subElemId "New definition"
            let dsts =
                    drop 1 (workArea ^.. Sugar.waPanes . traverse . Sugar.paneEntityId <&> WidgetIds.fromEntityId) ++ [newDefId]
            panesEdits <-
                workArea ^. Sugar.waPanes
                & zipWithM (makePaneEdit theExportActions) dsts
            newDefinitionButton <-
                makeNewDefinitionButton cp newDefId
                <&> Widget.updates %~ IOTrans.liftTrans
                <&> Responsive.fromWidget
            eventMap <- panesEventMap theExportActions cp gp
            Responsive.vboxSpaced (panesEdits <> [newDefinitionButton])
                <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> eventMap)
            & GuiM.run (workArea ^. Sugar.waOpenPane) assocTagName ExpressionEdit.make BinderEdit.make
                (Anchors.onGui (Property.mkProperty %~ lift) gp) env
            & lift
            <&> Responsive.render narrowParams
            <&> (^. Align.tValue)
            <&> (,) gotoDefinition
    where
        narrowParams =
            Responsive.NarrowLayoutParams
            { _layoutWidth = width
            , _layoutNeedDisambiguation = False
            }

exportPaneEventMap ::
    _ =>
    env -> ExportActions m -> Sugar.PaneBody v name i o dummy ->
    EventMap (IOTrans m Update)
exportPaneEventMap env theExportActions paneBody =
    case paneBody of
    Sugar.PaneDefinition def ->
        exportEventMap exportDef (def ^. Sugar.drDefI) Texts.exportDefToJSON exportKeys
        <> exportEventMap exportDefToJS (def ^. Sugar.drDefI) Texts.exportDefToJS
            (env ^. has . Config.export . Config.exportFancyKeys)
        <> execEventMap
        where
            execEventMap =
                case def ^? Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deVarInfo of
                Just ( Sugar.VarNominal (Sugar.TId _ tid)) | tid == Builtins.mutTid ->
                    E.keysEventMap (env ^. has . Config.export . Config.executeKeys)
                    (E.toDoc (env ^. has) [Texts.execRepl])
                    (IOTrans.liftIO (executeDef theExportActions (def ^. Sugar.drDefI)))
                _ -> mempty
    Sugar.PaneTag tag ->
        exportEventMap exportTag (tag ^. Sugar.tpTag) Texts.exportTagToJSON exportKeys
    Sugar.PaneNominal nom ->
        exportEventMap exportNominal (nom ^. Sugar.npNominalId) Texts.exportNominalToJSON exportKeys
    where
        exportKeys = env ^. has . Config.export . Config.exportKeys
        exportEventMap act arg docLens keys =
            act theExportActions arg
            & E.keyPresses keys
            (E.toDoc (env ^. has) [Texts.collaboration, docLens])

deleteAndClosePaneEventMap ::
    _ =>
    ElemId ->
    Sugar.Pane v name i0 o a -> Property o Sugar.DefinitionState ->
    GuiM env i1 o (EventMap (o Update))
deleteAndClosePaneEventMap prevId pane defState =
    Lens.view id
    <&> \env ->
    do
        (defState ^. Property.pSet) Sugar.DeletedDefinition
        prevId <$ pane ^. Sugar.paneClose
    & E.keysEventMapMovesCursor (Config.delKeys env)
    (E.toDoc env
        [ has . MomentuTexts.edit
        , has . Texts.def
        , has . MomentuTexts.delete
        ])

makePaneBodyEdit :: _ => ExprGui.Top Sugar.Pane i o -> GuiM env i o (Responsive (DefRes o))
makePaneBodyEdit pane =
    case pane ^. Sugar.paneBody of
    Sugar.PaneTag tag -> TagPaneEdit.make tag myId <&> Responsive.fromWidget <&> Widget.updates %~ lift
    Sugar.PaneDefinition def -> DefinitionEdit.make def myId
    Sugar.PaneNominal nom -> NominalPane.make nom <&> Widget.updates %~ lift
    where
        myId = pane ^. Sugar.paneEntityId & WidgetIds.fromEntityId

undeleteButton :: _ => o ElemId -> GuiM env i o (M.TextWidget o)
undeleteButton undelete =
    do
        actionId <- Element.subElemId "Undelete"
        toDoc <- Lens.view id <&> E.toDoc
        let doc =
                toDoc
                [ has . MomentuTexts.edit
                , has . Texts.def
                , has . Texts.undelete
                ]
        Styled.actionable actionId Texts.undeleteButton
            doc undelete

wholeFocused :: Widget.Size -> Widget.Focused a -> Widget.Focused a
wholeFocused size f =
    Widget.Focused
    { Widget._fFocalAreas = [Rect 0 size]
    , Widget._fEventMap = mempty
    , Widget._fPreEvents = mempty
    , Widget._fMEnterPoint = Nothing
    , Widget._fLayers = f ^. Widget.fLayers
    }

makePaneEdit ::
    _ =>
    ExportActions m ->
    ElemId ->
    ExprGui.Top Sugar.Pane (OnceT (T m)) (T m) ->
    GuiM env (OnceT (T m)) (T m) (Responsive (IOTrans m))
makePaneEdit theExportActions prevId pane =
    do
        env <- Lens.view id
        let titledCodeDoc titleLenses texts =
                E.toDoc env
                (titleLenses ++ map (has .) texts)
        let viewDoc = titledCodeDoc [has . MomentuTexts.view]
        let paneEventMap =
                [ prevId <$ IOTrans.liftTrans (pane ^. Sugar.paneClose)
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
        bodyGui <-
            makePaneBodyEdit pane <&> Widget.updates %~
            IOTrans . Compose . pure . Compose .
            fmap ((_1 %~ (`when` exec) . (^. Lens._Wrapped)) . swap) .
            runWriterT . (^. _DefRes)
        case pane ^. Sugar.paneDefinitionState . Property.pVal of
            Sugar.LiveDefinition ->
                deleteAndClosePaneEventMap prevId pane (pane ^. Sugar.paneDefinitionState)
                <&> Lens.mapped %~ IOTrans.liftTrans
                <&> (`Widget.weakerEvents` bodyGui)
            Sugar.DeletedDefinition ->
                do
                    buttonGui <-
                        paneId
                        <$ (pane ^. Sugar.paneDefinitionState . Property.pSet) Sugar.LiveDefinition
                        & undeleteButton <&> Responsive.fromWithTextPos
                        <&> Widget.updates %~ IOTrans.liftTrans
                    g <-
                        bodyGui
                        & Responsive.alignedWidget . M.tValue .> Widget.wFocused %@~ wholeFocused
                        & Styled.deletedDef
                    Responsive.vbox [buttonGui, g]
                & local (M.elemIdPrefix <>~ M.asElemId paneId)
            <&> Widget.weakerEvents paneEventMap
    where
        paneId = WidgetIds.fromEntityId (pane ^. Sugar.paneEntityId)
        exec =
            traverse_ (executeDef theExportActions)
            (pane ^? Sugar.paneBody . Sugar._PaneDefinition . Sugar.drDefI)

makeNewDefinition :: _ => Anchors.CodeAnchors m -> GuiM env (OnceT (T m)) (T m) (T m (DefI m))
makeNewDefinition cp =
    GuiM.mkPrejumpPosSaver <&> (*> DataOps.newEmptyPublicDefinitionWithPane cp)

newDefinitionDoc :: _ => m E.Doc
newDefinitionDoc =
    Lens.view id
    <&> (`E.toDoc` [has . MomentuTexts.edit, has . Texts.new])

makeNewDefinitionButton :: _ => Anchors.CodeAnchors m -> ElemId -> GuiM env (OnceT (T m)) (T m) (Widget (T m))
makeNewDefinitionButton cp newDefId =
    do
        newDefDoc <- newDefinitionDoc
        action <- makeNewDefinition cp
        let nameNewDef c =
                action
                <&> (`ofTaggedEntity` Anchors.anonTag)
                <&> WidgetIds.fromEntityId <&> WidgetIds.tagHoleId
                <&> SearchMenu.enterWithSearchTerm (Lens._Cons # (c, ""))
        Styled.actionable newDefId Texts.newDefinitionButton newDefDoc
            (action <&> WidgetIds.fromIRef)
            <&> (^. Align.tValue)
            <&> Widget.weakerEvents (E.allChars "Character" newDefDoc nameNewDef)

jumpBack :: Monad m => Anchors.GuiAnchors (T m) (T m) -> T m (Maybe (T m ElemId))
jumpBack gp =
    Property.getP (Anchors.preJumps gp)
    <&> \case
    [] -> Nothing
    (j:js) -> j <$ Property.setP (Anchors.preJumps gp) js & Just

panesEventMap ::
    _ =>
    ExportActions m -> Anchors.CodeAnchors m -> Anchors.GuiAnchors (T m) (T m) ->
    GuiM env (OnceT (T m)) (T m) (EventMap (IOTrans m Update))
panesEventMap theExportActions cp gp =
    do
        env <- Lens.view id
        let exportConfig = env ^. has . Config.export
        mJumpBack <- jumpBack gp & transaction <&> fmap IOTrans.liftTrans
        newDefDoc <- newDefinitionDoc
        newDefinitionEventMap <-
            makeNewDefinition cp
            <&> Lens.mapped %~ WidgetIds.fromIRef
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
            , E.keyPresses (exportConfig ^. Config.exportAllKeys)
              (collaborationDoc [Texts.collaboration, Texts.exportEverythingToJSON])
                exportAll
            ] & pure
    where
        ExportActions{importAll,exportAll} = theExportActions
