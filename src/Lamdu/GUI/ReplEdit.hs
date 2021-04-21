-- | REPL Edit
{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           Control.Monad.Once (OnceT)
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurPrevTag(..), curPrevTag, fallbackToPrev)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.Monad (GuiM, makeBinder)
import           Lamdu.GUI.Styled (label)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Collaboration as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Navigation as Texts
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
    _ => env -> Sugar.Payload v (T m) -> [M.MetaKey] -> EventMap (T m M.Update)
extractEventMap env pl keys =
    pl ^. Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor & E.keysEventMapMovesCursor keys doc
    where
        doc =
            E.toDoc env
            [has . MomentuTexts.edit, has . Texts.extractReplToDef]

replEventMap ::
    _ => env -> ExportRepl m -> Sugar.Payload v (T m) -> EventMap (IOTrans m M.Update)
replEventMap env (ExportRepl expRepl expFancy _execRepl) replExprPl =
    mconcat
    [ extractEventMap env replExprPl (env ^. has . Config.extractKeys)
        <&> IOTrans.liftTrans
    , E.keysEventMap (exportConfig ^. Config.exportKeys)
      (toDoc [Texts.exportReplToJSON]) expRepl
    , E.keysEventMap (exportConfig ^. Config.exportFancyKeys)
      (toDoc [Texts.exportReplToJS]) expFancy
    ]
    where
        toDoc = E.toDoc (env ^. has)
        exportConfig = env ^. has . Config.export

indicatorColor :: _ => CurPrevTag -> Lens' Theme M.Color -> m M.Color
indicatorColor Current color = Lens.view (has . color)
indicatorColor Prev _ = Lens.view (has . Theme.disabledColor)

makeIndicator :: _ => CurPrevTag -> Lens' Theme M.Color -> Text -> m (M.WithTextPos M.View)
makeIndicator tag enabledColor text =
    do
        color <- indicatorColor tag enabledColor
        Label.make text & Reader.local (TextView.color .~ color)

compiledErrorDesc :: Sugar.CompiledErrorType -> OneOf Texts.CodeUI
compiledErrorDesc Sugar.ReachedHole = Texts.jsReachedAHole
compiledErrorDesc Sugar.DependencyTypeOutOfDate = Texts.jsStaleDep
compiledErrorDesc Sugar.UnhandledCase = Texts.jsUnhandledCase

errorDesc :: _ => Sugar.Error -> m (M.WithTextPos M.View)
errorDesc err =
    do
        errorColor <- Lens.view (has . Theme.errorColor)
        case err of
            Sugar.CompiledError cErr ->
                label (compiledErrorDesc cErr)
            Sugar.RuntimeError exc ->
                label Texts.jsException
                M./|/ ((TextView.make ?? exc)
                        <*> (Element.subAnimId ?? ["exception text"]))
            & Reader.local (TextView.color .~ errorColor)

errorIndicator :: _ => Widget.Id -> CurPrevTag -> Sugar.EvalException o -> m (M.TextWidget o)
errorIndicator myId tag (Sugar.EvalException errorType jumpToErr) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        env <- Lens.view id
        let jumpDoc =
                E.toDoc env
                [has . MomentuTexts.navigation, has . Texts.jumpToError]
        let jumpEventMap j =
                j <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor actionKeys jumpDoc
        indicator <-
            (Widget.makeFocusableView ?? myId <&> (M.tValue %~))
            <*> makeIndicator tag Theme.errorColor  "⚠"
            <&> Lens.mapped %~ Widget.weakerEvents (foldMap jumpEventMap jumpToErr)
        if Widget.isFocused (indicator ^. M.tValue)
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
                        ] <&> (^. M.tValue)
                anchor indicator
                    <&> Hover.hoverInPlaceOf hoverOptions
                    & pure
            else
                pure indicator

indicatorId :: Widget.Id
indicatorId = Widget.joinId WidgetIds.replId ["result indicator"]

resultWidget ::
    _ =>
    ExportRepl o -> Sugar.VarInfo -> CurPrevTag -> Sugar.EvalCompletionResult (T o) ->
    m (M.TextWidget (IOTrans o))
resultWidget expRepl varInfo tag Sugar.EvalSuccess{} =
    do
        view <- makeIndicator tag Theme.successColor "✔"
        toDoc <- Lens.view has <&> E.toDoc
        case varInfo of
            Sugar.VarNominal (Sugar.TId _ tid) | tid == Builtins.mutTid ->
                do
                    actionKeys <- Lens.view (has . Config.actionKeys)
                    let executeEventMap =
                            executeIOProcess expRepl
                            & IOTrans.liftIO
                            & E.keysEventMap actionKeys (toDoc [Texts.execRepl])
                    Widget.makeFocusableView ?? indicatorId <&> (M.tValue %~) ?? view
                        <&> M.tValue %~ Widget.weakerEvents executeEventMap
            _ -> view & M.tValue %~ Widget.fromView & pure
resultWidget _ _ tag (Sugar.EvalError err) =
    errorIndicator indicatorId tag err
    <&> M.tValue . Widget.updates %~ IOTrans.liftTrans

noIndicator :: _ => m (w a)
noIndicator = Widget.respondToCursorPrefix ?? indicatorId ?? M.empty

make ::
    _ =>
    ExportRepl m ->
    ExprGui.Top Sugar.Repl (OnceT (T m)) (T m) ->
    GuiM env (OnceT (T m)) (T m) (Responsive (IOTrans m))
make expRepl (Sugar.Repl replExpr varInfo replResult) =
    do
        env <- Lens.view id
        let buttonExtractKeys = env ^. has . Config.actionKeys
        result <-
            (resultWidget expRepl varInfo <$> curPrevTag <&> fmap) <*> replResult
            & fallbackToPrev
            & fromMaybe (noIndicator <&> M.WithTextPos 0)
            & Reader.local (M.animIdPrefix <>~ ["result widget"])
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        let centeredBelow down up =
                (M.Aligned 0.5 up |---| M.Aligned 0.5 down) ^. M.value
        let extractEvents = extractEventMap env replExprPl buttonExtractKeys
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? replSymId <&> (M.tValue %~))
              <*> label Texts.repl
              <&> Lens.mapped %~ Widget.weakerEvents extractEvents
              <&> Lens.mapped . Widget.updates %~ IOTrans.liftTrans
              <&> centeredBelow result
              <&> Responsive.fromWithTextPos
            , makeBinder replExpr <&> Widget.updates %~ IOTrans.liftTrans
            ]
            <&> Widget.weakerEvents (replEventMap env expRepl replExprPl)
            & GuiState.assignCursor WidgetIds.replId replExprId
    where
        replSymId = Widget.joinId WidgetIds.replId ["symbol"]
        replExprPl = replExpr ^. SugarLens.binderResultExpr . _1
        replExprId = WidgetIds.fromExprPayload replExprPl
