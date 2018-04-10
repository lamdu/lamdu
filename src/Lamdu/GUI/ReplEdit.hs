-- | REPL Edit
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings, FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurPrevTag(..), curPrevTag, fallbackToPrev)
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           GUI.Momentu.Align (Aligned(..), value)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM')
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
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
    Functor m =>
    Sugar.Expression name (T m) (T m) a -> [MetaKey] ->
    EventMap (T m GuiState.Update)
extractEventMap replExpr keys =
    replExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor
    & E.keysEventMapMovesCursor keys (E.Doc ["Edit", "Extract to definition"])

replEventMap ::
    Monad m =>
    Config -> ExportRepl m ->
    Sugar.Expression name (T m) (T m) a -> EventMap (IOTrans m GuiState.Update)
replEventMap theConfig (ExportRepl exportRepl exportFancy _execRepl) replExpr =
    mconcat
    [ extractEventMap replExpr (theConfig ^. Config.extractKeys)
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

indicator ::
    ( MonadReader env m, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    CurPrevTag -> Lens' Theme Draw.Color -> Text -> m (Align.WithTextPos View)
indicator tag enabledColor text =
    do
        color <- indicatorColor tag enabledColor
        TextView.makeLabel text & Reader.local (TextView.color .~ color)


resultWidget ::
    ( MonadReader env m
    , Element.HasAnimIdPrefix env, TextView.HasStyle env, HasTheme env
    ) =>
    CurPrevTag -> Maybe (Sugar.EvalCompletionResult name o) ->
    Maybe (m (Align.WithTextPos View))
resultWidget _ Nothing = Nothing
resultWidget tag (Just Sugar.EvalSuccess {}) =
    indicator tag Theme.successColor "✔" & Just
resultWidget tag (Just (Sugar.EvalError _err)) =
    indicator tag Theme.errorColor  "⚠" & Just

make ::
    Monad m =>
    ExportRepl m ->
    Sugar.Repl (Name (T m)) (T m) (T m) ExprGui.Payload ->
    ExprGuiM' (T m) (Responsive (IOTrans m GuiState.Update))
make exportRepl (Sugar.Repl replExpr replResult) =
    do
        theConfig <- Lens.view config
        let buttonExtractKeys = theConfig ^. Config.actionKeys
        result <-
            resultWidget <$> curPrevTag <*> replResult
            & fallbackToPrev
            & sequenceA
            & Reader.local (Element.animIdPrefix <>~ ["result widget"])
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"] <&> (Align.tValue %~))
              <*> TextView.makeLabel "⋙"
              <&> Lens.mapped %~ Widget.weakerEvents (extractEventMap replExpr buttonExtractKeys)
              <&> maybe id centeredBelow result
              <&> Responsive.fromWithTextPos
            , ExprGuiM.makeSubexpression replExpr
            ]
            <&> Lens.mapped %~ IOTrans.liftTrans
            <&> Widget.weakerEvents (replEventMap theConfig exportRepl replExpr)
            & GuiState.assignCursor WidgetIds.replId replExprId
    where
        centeredBelow down up = (Aligned 0.5 up /-/ Aligned 0.5 down) ^. value
        replExprId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId
