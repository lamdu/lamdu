-- | REPL Edit
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make, isExecutableType
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurPrevTag(..), curPrevTag, fallbackToPrev)
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           GUI.Momentu.Align (Aligned(..), value, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Config (Config, HasConfig(..))
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.BinderEdit (makeBinderBodyEdit)
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.WidgetIds as WidgetIds
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
    Functor m => Sugar.Payload name i (T m) a -> [MetaKey] -> Gui EventMap (T m)
extractEventMap pl keys =
    pl ^. Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor
    & E.keysEventMapMovesCursor keys (E.Doc ["Edit", "Extract to definition"])

replEventMap ::
    Monad m =>
    Config -> ExportRepl m -> Sugar.Payload name i (T m) a ->
    Gui EventMap (IOTrans m)
replEventMap theConfig (ExportRepl exportRepl exportFancy _execRepl) replExprPl =
    mconcat
    [ extractEventMap replExprPl (theConfig ^. Config.extractKeys)
        <&> IOTrans.liftTrans
    , E.keysEventMap (exportConfig ^. Config.exportKeys)
      (E.Doc ["Collaboration", "Export repl to JSON file"]) exportRepl
    , E.keysEventMap (exportConfig ^. Config.exportFancyKeys)
      (E.Doc ["Collaboration", "Export repl as JS"]) exportFancy
    ]
    where
        exportConfig = theConfig ^. Config.export

indicatorColor ::
    (MonadReader env m, HasTheme env) =>
    CurPrevTag -> Lens' Theme Draw.Color -> m Draw.Color
indicatorColor Current color = Lens.view (theme . color)
indicatorColor Prev _ = Lens.view (theme . Theme.disabledColor)

makeIndicator ::
    ( MonadReader env m, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    CurPrevTag -> Lens' Theme Draw.Color -> Text -> m (Align.WithTextPos View)
makeIndicator tag enabledColor text =
    do
        color <- indicatorColor tag enabledColor
        TextView.makeLabel text & Reader.local (TextView.color .~ color)

errorIndicator ::
    ( MonadReader env m, Applicative o, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Hover.HasStyle env, GuiState.HasCursor env
    , HasTheme env, HasConfig env
    ) =>
    Widget.Id -> CurPrevTag -> Sugar.EvalException o ->
    m (Align.TextWidget o)
errorIndicator myId tag (Sugar.EvalException errorType desc jumpToErr) =
    do
        actionKeys <- Lens.view (Config.config . Config.actionKeys)
        let jumpEventMap =
                case jumpToErr of
                Nothing -> mempty
                Just j ->
                    j <&> dest
                    & E.keysEventMapMovesCursor actionKeys jumpDoc
        indicator <-
            (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> makeIndicator tag Theme.errorColor  "⚠"
            <&> Lens.mapped %~ Widget.weakerEvents jumpEventMap
        if Widget.isFocused (indicator ^. Align.tValue)
            then
            do
                errorColor <- Lens.view (theme . Theme.errorColor)
                descLabel <-
                    TextView.makeLabel desc & Reader.local (TextView.color .~ errorColor)
                hspace <- Spacer.stdHSpace
                vspace <- Spacer.stdVSpace
                hover <- Hover.hover
                let hDescLabel f = hover (f descLabel) & Hover.sequenceHover
                let hoverOptions =
                        [ anchor indicator /|/ hDescLabel (hspace /|/)
                        , anchor indicator /-/ hDescLabel (vspace /-/)
                        ] <&> (^. Align.tValue)
                anchor indicator
                    <&> Hover.hoverInPlaceOf hoverOptions
                    & pure
            else
                pure indicator
    where
        dest entityId =
            case errorType of
            Sugar.ReachedHole -> HoleWidgetIds.make entityId & HoleWidgetIds.hidClosed
            _ -> WidgetIds.fromEntityId entityId
        jumpDoc = E.Doc ["Navigation", "Jump to error"]
        anchor = fmap Hover.anchor

indicatorId :: Sugar.Payload name i o a -> Widget.Id
indicatorId pl =
    WidgetIds.fromEntityId (pl ^. Sugar.plEntityId) `Widget.joinId` ["result indicator"]

isExecutableType :: Sugar.Type name -> Bool
isExecutableType t =
    case t ^. Sugar.tBody of
    Sugar.TInst tid _ -> tid ^. Sugar.tidTId == Builtins.mutTid
    _ -> False

resultWidget ::
    ( MonadReader env m, GuiState.HasCursor env, Monad o
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env, Hover.HasStyle env
    , HasTheme env, HasConfig env
    ) =>
    ExportRepl o -> Sugar.Payload name i (T o) a -> CurPrevTag -> Sugar.EvalCompletionResult name (T o) ->
    m (TextWidget (IOTrans o))
resultWidget exportRepl pl tag Sugar.EvalSuccess {} =
    do
        view <- makeIndicator tag Theme.successColor "✔"
        if Lens.anyOf (Sugar.plAnnotation . SugarLens.annotationTypes) isExecutableType pl
            then
                do
                    actionKeys <- Lens.view (Config.config . Config.actionKeys)
                    let executeEventMap =
                            executeIOProcess exportRepl
                            & IOTrans.liftIO
                            & E.keysEventMap actionKeys (E.Doc ["Execute"])
                    (Widget.makeFocusableView ?? indicatorId pl <&> (Align.tValue %~)) ?? view
                        <&> Align.tValue %~ Widget.weakerEvents executeEventMap
            else
                view & Align.tValue %~ Widget.fromView & pure
resultWidget _ pl tag (Sugar.EvalError err) =
    errorIndicator (indicatorId pl) tag err
    <&> Align.tValue . Lens.mapped %~ IOTrans.liftTrans

make ::
    Monad m =>
    ExportRepl m ->
    Sugar.Repl (Name (T m)) (T m) (T m)
    (Sugar.Payload (Name (T m)) (T m) (T m) ExprGui.Payload) ->
    ExprGuiM (T m) (T m) (Gui Responsive (IOTrans m))
make exportRepl (Sugar.Repl replExpr _varInfo replResult) =
    do
        theConfig <- Lens.view config
        let buttonExtractKeys = theConfig ^. Config.actionKeys
        result <-
            (resultWidget exportRepl replExprPl <$> curPrevTag <&> fmap) <*> replResult
            & fallbackToPrev
            & sequenceA
            & Reader.local (Element.animIdPrefix <>~ ["result widget"])
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"] <&> (Align.tValue %~))
              <*> TextView.makeLabel "⋙"
              <&> Lens.mapped %~ Widget.weakerEvents (extractEventMap replExprPl buttonExtractKeys)
              <&> Lens.mapped . Lens.mapped %~ IOTrans.liftTrans
              <&> maybe id centeredBelow result
              <&> Responsive.fromWithTextPos
            , makeBinderBodyEdit replExpr
                <&> Lens.mapped %~ IOTrans.liftTrans
            ]
            <&> Widget.weakerEvents (replEventMap theConfig exportRepl replExprPl)
            & GuiState.assignCursor WidgetIds.replId replExprId
    where
        centeredBelow down up = (Aligned 0.5 up /-/ Aligned 0.5 down) ^. value
        replExprPl = replExpr ^. SugarLens.binderResultExpr
        replExprId = WidgetIds.fromExprPayload replExprPl
