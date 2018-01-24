-- | REPL Edit
{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make
    ) where

import qualified Control.Lens as Lens
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config, config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

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
    Sugar.Expression name (T m) a -> [MetaKey] -> EventMap (T m GuiState.Update)
extractEventMap replExpr keys =
    replExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor
    & E.keysEventMapMovesCursor keys (E.Doc ["Edit", "Extract to definition"])

replEventMap ::
    Monad m =>
    Config -> ExportRepl m ->
    Sugar.Expression name (T m) a -> EventMap (IOTrans m GuiState.Update)
replEventMap theConfig (ExportRepl exportRepl exportFancy executeRepl) replExpr =
    mconcat
    [ extractEventMap replExpr (Config.extractKeys theConfig) <&> IOTrans.liftTrans
    , E.keysEventMap exportKeys
      (E.Doc ["Collaboration", "Export repl to JSON file"]) exportRepl
    , E.keysEventMap exportFancyKeys
      (E.Doc ["Collaboration", "Export repl for Codejam"]) exportFancy
    , case replExpr ^. Sugar.rPayload . Sugar.plAnnotation . Sugar.aInferredType of
        T.TInst tid _
            | tid == Builtins.mutTid ->
                E.keysEventMap executeKeys (E.Doc ["Execute Repl Process"])
                (IOTrans (pure (pure mempty) <$ executeRepl))
        _ -> mempty
    ]
    where
        Config.Export{exportKeys, exportFancyKeys, executeKeys} = Config.export theConfig

make ::
    Monad m =>
    ExportRepl m ->
    ExprGui.SugarExpr m ->
    ExprGuiM m (Responsive (IOTrans m GuiState.Update))
make exportRepl replExpr =
    do
        theConfig <- Lens.view config
        let buttonExtractKeys = Config.newDefinitionButtonPressKeys (Config.pane theConfig)
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"] <&> (Align.tValue %~))
              <*> TextView.makeLabel "⋙"
              <&> Lens.mapped %~ Widget.weakerEvents (extractEventMap replExpr buttonExtractKeys)
              <&> Responsive.fromWithTextPos
            , ExprGuiM.makeSubexpression replExpr
            ]
            <&> Lens.mapped %~ IOTrans.liftTrans
            <&> Widget.weakerEvents (replEventMap theConfig exportRepl replExpr)
            & GuiState.assignCursor WidgetIds.replId exprId
    where
        exprId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId
