-- | REPL Edit
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make, isExecutableType
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurPrevTag(..), curPrevTag, fallbackToPrev)
import           Data.Has (Has(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           GUI.Momentu.Align (Aligned(..), value, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Config (Config(..))
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, makeBinder)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.Styled (label)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ExportRepl m = ExportRepl
    { exportRepl :: IOTrans m ()
    , -- Fancy export is intended for sending code to someone who doesn't have
      -- Lamdu installed. It bundles together in a zipfile a screenshot,
      -- a README, the repl export, and compiled JS  (that requires a new
      -- version of nodejs supporting TCO).  It is intended to enable Lamdu
      -- to be used in competitions such as Google codejam which require
      -- [readable] code upload.
      exportFancy :: IOTrans m ()
    , -- Executing code is in a way a form of exporting.
      executeIOProcess :: IO ()
    }

extractEventMap ::
    (Language.HasLanguage env, Functor m) =>
    env -> Sugar.Payload name i (T m) a -> [MetaKey] -> Gui EventMap (T m)
extractEventMap env pl keys =
    pl ^. Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor & E.keysEventMapMovesCursor keys doc
    where
        doc =
            E.toDoc (env ^. Language.texts)
            [Texts.edit, Texts.definitions . Texts.extractReplToDef]

replEventMap ::
    (Monad m, Has Config env, Language.HasLanguage env) =>
    env -> ExportRepl m -> Sugar.Payload name i (T m) a ->
    Gui EventMap (IOTrans m)
replEventMap env (ExportRepl exportRepl exportFancy _execRepl) replExprPl =
    mconcat
    [ extractEventMap env replExprPl (env ^. has . Config.extractKeys)
        <&> IOTrans.liftTrans
    , E.keysEventMap (exportConfig ^. Config.exportKeys)
      (toDoc [Texts.collaboration, Texts.exportReplToJSON]) exportRepl
    , E.keysEventMap (exportConfig ^. Config.exportFancyKeys)
      (toDoc [Texts.collaboration, Texts.exportReplToJS]) exportFancy
    ]
    where
        toDoc = E.toDoc (env ^. Language.texts . Texts.collaborationTexts)
        exportConfig = env ^. has . Config.export

indicatorColor ::
    (MonadReader env m, Has Theme env) =>
    CurPrevTag -> Lens' Theme Draw.Color -> m Draw.Color
indicatorColor Current color = Lens.view (has . color)
indicatorColor Prev _ = Lens.view (has . Theme.disabledColor)

makeIndicator ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env
    ) =>
    CurPrevTag -> Lens' Theme Draw.Color -> Text -> m (Align.WithTextPos View)
makeIndicator tag enabledColor text =
    do
        color <- indicatorColor tag enabledColor
        Label.make text & Reader.local (TextView.color .~ color)

compiledErrorDesc :: Sugar.CompiledErrorType -> OneOf Texts.CodeUI
compiledErrorDesc Sugar.ReachedHole = Texts.jsReachedAHole
compiledErrorDesc Sugar.DependencyTypeOutOfDate = Texts.jsStaleDep
compiledErrorDesc Sugar.UnhandledCase = Texts.jsUnhandledCase

errorDesc ::
    ( MonadReader env m, Has Theme env, Language.HasLanguage env
    , Element.HasAnimIdPrefix env, Has TextView.Style env
    ) =>
    Sugar.Error -> m (Align.WithTextPos View)
errorDesc err =
    do
        errorColor <- Lens.view (has . Theme.errorColor)
        case err of
            Sugar.CompiledError cErr ->
                label (Texts.codeUI . compiledErrorDesc cErr)
            Sugar.RuntimeError exc ->
                label (Texts.codeUI . Texts.jsException)
                /|/ ((TextView.make ?? exc)
                        <*> (Element.subAnimId ?? ["exception text"]))
            & Reader.local (TextView.color .~ errorColor)

errorIndicator ::
    ( MonadReader env m, Applicative o, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Has Hover.Style env, GuiState.HasCursor env
    , Has Theme env, Has Config env, Glue.HasTexts env, Language.HasLanguage env
    ) =>
    Widget.Id -> CurPrevTag -> Sugar.EvalException o ->
    m (Align.TextWidget o)
errorIndicator myId tag (Sugar.EvalException errorType jumpToErr) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        env <- Lens.view id
        let jumpDoc =
                E.toDoc (env ^. Language.texts)
                [Texts.navigation, Texts.navigationTexts . Texts.jumpToError]
        let jumpEventMap j =
                j <&> dest
                & E.keysEventMapMovesCursor actionKeys jumpDoc
        indicator <-
            (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> makeIndicator tag Theme.errorColor  "⚠"
            <&> Lens.mapped %~ Widget.weakerEvents (foldMap jumpEventMap jumpToErr)
        if Widget.isFocused (indicator ^. Align.tValue)
            then
            do
                descLabel <- errorDesc errorType
                hspace <- Spacer.stdHSpace
                vspace <- Spacer.stdVSpace
                hover <- Hover.hover
                Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
                Glue.Poly (|---|) <- Glue.mkPoly ?? Glue.Vertical
                anchor <- Hover.anchor <&> fmap
                let hDescLabel f = hover (f descLabel) & Hover.sequenceHover
                let hoverOptions =
                        [ anchor indicator ||| hDescLabel (hspace |||)
                        , anchor indicator |---| hDescLabel (vspace |---|)
                        ] <&> (^. Align.tValue)
                anchor indicator
                    <&> Hover.hoverInPlaceOf hoverOptions
                    & pure
            else
                pure indicator
    where
        dest entityId =
            case errorType of
            Sugar.CompiledError Sugar.ReachedHole ->
                HoleWidgetIds.make entityId & HoleWidgetIds.hidClosed
            _ -> WidgetIds.fromEntityId entityId

indicatorId :: Widget.Id
indicatorId = Widget.joinId WidgetIds.replId ["result indicator"]

isExecutableType :: Sugar.Type name -> Bool
isExecutableType t =
    case t ^. Sugar.tBody of
    Sugar.TInst tid _ -> tid ^. Sugar.tidTId == Builtins.mutTid
    _ -> False

resultWidget ::
    ( MonadReader env m, GuiState.HasCursor env, Monad o
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env, Has Hover.Style env
    , Has Theme env, Has Config env, Language.HasLanguage env
    ) =>
    ExportRepl o -> Sugar.VarInfo -> CurPrevTag -> Sugar.EvalCompletionResult name (T o) ->
    m (TextWidget (IOTrans o))
resultWidget exportRepl varInfo tag Sugar.EvalSuccess{} =
    do
        view <- makeIndicator tag Theme.successColor "✔"
        toDoc <- Lens.view (Language.texts . Texts.definitions) <&> E.toDoc
        case varInfo of
            Sugar.VarAction ->
                do
                    actionKeys <- Lens.view (has . Config.actionKeys)
                    let executeEventMap =
                            executeIOProcess exportRepl
                            & IOTrans.liftIO
                            & E.keysEventMap actionKeys (toDoc [Texts.execRepl])
                    (Widget.makeFocusableView ?? indicatorId <&> (Align.tValue %~)) ?? view
                        <&> Align.tValue %~ Widget.weakerEvents executeEventMap
            _ -> view & Align.tValue %~ Widget.fromView & pure
resultWidget _ _ tag (Sugar.EvalError err) =
    errorIndicator indicatorId tag err
    <&> Align.tValue . Lens.mapped %~ IOTrans.liftTrans

make ::
    Monad m =>
    ExportRepl m ->
    Sugar.Repl (Name (T m)) (T m) (T m)
    (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload) ->
    ExprGuiM (T m) (T m) (Gui Responsive (IOTrans m))
make exportRepl (Sugar.Repl replExpr varInfo replResult) =
    do
        env <- Lens.view id
        let buttonExtractKeys = env ^. has . Config.actionKeys
        result <-
            (resultWidget exportRepl varInfo <$> curPrevTag <&> fmap) <*> replResult
            & fallbackToPrev
            & sequenceA
            & Reader.local (Element.animIdPrefix <>~ ["result widget"])
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        let centeredBelow down up =
                (Aligned 0.5 up |---| Aligned 0.5 down) ^. value
        let extractEvents = extractEventMap env replExprPl buttonExtractKeys
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? replSymId <&> (Align.tValue %~))
              <*> label (Texts.code . Texts.repl)
              <&> Lens.mapped %~ Widget.weakerEvents extractEvents
              <&> Lens.mapped . Lens.mapped %~ IOTrans.liftTrans
              <&> maybe id centeredBelow result
              <&> Responsive.fromWithTextPos
            , makeBinder replExpr
                <&> Lens.mapped %~ IOTrans.liftTrans
            ]
            <&> Widget.weakerEvents (replEventMap env exportRepl replExprPl)
            & GuiState.assignCursor WidgetIds.replId replExprId
    where
        replSymId = Widget.joinId WidgetIds.replId ["symbol"]
        replExprPl = replExpr ^. SugarLens.binderResultExpr
        replExprId = WidgetIds.fromExprPayload replExprPl
